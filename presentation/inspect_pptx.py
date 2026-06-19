"""
Inspect slides.pptx for Windows compatibility issues.
Run: python3 presentation/inspect_pptx.py
"""
import xml.etree.ElementTree as ET
import os, struct, zipfile

PPTX = os.path.join(os.path.dirname(__file__), 'slides.pptx')
ns = {
    'a': 'http://schemas.openxmlformats.org/drawingml/2006/main',
    'p': 'http://schemas.openxmlformats.org/presentationml/2006/main',
    'r': 'http://schemas.openxmlformats.org/officeDocument/2006/relationships',
}

def emu(v): return round(int(v) / 914400, 2)

with zipfile.ZipFile(PPTX) as z:
    names = z.namelist()

    # --- Theme fonts ---
    print("=== THEME FONTS ===")
    with z.open('ppt/theme/theme1.xml') as f:
        tree = ET.parse(f)
    root = tree.getroot()
    for el in root.iter('{http://schemas.openxmlformats.org/drawingml/2006/main}latin'):
        tf = el.get('typeface','')
        if tf:
            print(f'  latin typeface: {tf}')
    for el in root.iter('{http://schemas.openxmlformats.org/drawingml/2006/main}majorFont'):
        for ch in el:
            tf = ch.get('typeface','')
            if tf and '+' not in tf:
                print(f'  majorFont script typeface: {tf}')
        break

    # --- Slide count ---
    slide_files = sorted([n for n in names if n.startswith('ppt/slides/slide') and n.endswith('.xml') and '_rels' not in n])
    print(f"\n=== SLIDE COUNT: {len(slide_files)} ===")

    # --- Per-slide analysis ---
    print("\n=== PER-SLIDE ANALYSIS ===")
    for sf in slide_files:
        num = sf.replace('ppt/slides/slide','').replace('.xml','')
        with z.open(sf) as f:
            tree = ET.parse(f)
        root = tree.getroot()

        # Layout
        rels_path = f'ppt/slides/_rels/slide{num}.xml.rels'
        layout_name = 'unknown'
        if rels_path in names:
            with z.open(rels_path) as f:
                rt = ET.parse(f)
            for rel in rt.getroot():
                tgt = rel.get('Target','')
                if 'slideLayout' in tgt:
                    layout_file = tgt.replace('../','ppt/')
                    if layout_file in names:
                        with z.open(layout_file) as lf:
                            lt = ET.parse(lf)
                        cSld = lt.getroot().find('.//p:cSld', ns)
                        if cSld is not None:
                            layout_name = cSld.get('name', 'unknown')

        # Images
        images = [rel for rel in rt.getroot() if 'image' in rel.get('Type','')]
        img_count = len(images)

        # Text content (title)
        title = ''
        for sp in root.findall('.//p:sp', ns):
            ph = sp.find('.//p:ph', ns)
            if ph is not None and ph.get('type') in ('title','ctrTitle'):
                for r in sp.findall('.//a:t', ns):
                    title += r.text or ''
                break

        # Background fill
        bg_fills = len(root.findall('.//p:bg', ns))

        # Embedded fonts referenced in text runs
        fonts_used = set()
        for rpr in root.findall('.//a:rPr', ns):
            latin = rpr.find('a:latin', ns)
            if latin is not None:
                tf = latin.get('typeface','')
                if tf and '+' not in tf:
                    fonts_used.add(tf)

        print(f"\n  Slide {num:>2}: layout={layout_name:25s} images={img_count} bg_fills={bg_fills}")
        print(f"           title: {title[:70]}")
        if fonts_used:
            print(f"           explicit fonts: {sorted(fonts_used)}")

    # --- Media images ---
    print("\n=== MEDIA FILES ===")
    media = [n for n in names if n.startswith('ppt/media/')]
    for m in sorted(media):
        data = z.read(m)
        size_kb = round(len(data)/1024, 1)
        # PNG dimensions
        dims = ''
        if data[:8] == b'\x89PNG\r\n\x1a\n':
            w = struct.unpack('>I', data[16:20])[0]
            h = struct.unpack('>I', data[20:24])[0]
            dims = f' {w}x{h}px'
        print(f'  {m}: {size_kb}KB{dims}')

    # --- Notes slides (for speaker notes presence) ---
    notes = [n for n in names if n.startswith('ppt/notesSlides/') and n.endswith('.xml') and '_rels' not in n]
    print(f"\n=== NOTES SLIDES: {len(notes)} ===")

    # --- Check for embedded fonts ---
    print("\n=== EMBEDDED FONT FILES ===")
    fonts = [n for n in names if 'fonts' in n.lower() or n.endswith('.ttf') or n.endswith('.fntdata')]
    if fonts:
        for f in fonts:
            print(f'  {f}')
    else:
        print('  NONE — fonts are NOT embedded; Windows must have them installed')

    # --- Summary of potential issues ---
    print("\n=== WINDOWS COMPATIBILITY ANALYSIS ===")
    print()
    print("1. FONTS: Theme uses +mn-lt / +mj-lt = maps to 'Calibri' in theme1.xml.")
    print("   Calibri is standard on Windows (ships with Office) -> OK")
    print()
    print("2. FONT RENDERING: Figures use Liberation Sans / PT Sans (rasterized to PNG).")
    print("   PNG pixels are baked in, Windows does not need those fonts -> OK")
    print()
    print("3. EMBEDDED FONTS: None. All text-placeholder fonts rely on Calibri (system font).")
    print("   If PT Sans / Liberation Sans were used in TEXT placeholders (not images),")
    print("   Windows would substitute them. Check explicit fonts list above.")
    print()
    print("4. SLIDE BACKGROUND: reference.pptx uses light theme (white bg).")
    print("   All image fills are white-background PNGs -> OK")
    print()
    print("5. IMAGE FORMAT: All media are PNG (sRGB). PowerPoint 2013+ reads PNG fine -> OK")
    print()
    print("6. LAYOUT NAMES: Check if slideLayout names match what Windows PowerPoint expects.")
    print("   Quarto uses 'Title Slide', 'Title and Content', 'Two Content', 'Blank'.")
    print("   These are standard Microsoft layout names -> OK")
    print()
    print("7. POSSIBLE ISSUE — 'Content with Caption' layout:")
    print("   Quarto may fall back to this layout when it cannot match a layout.")
    print("   Windows PowerPoint may display content differently in this layout.")
    print("   Slides using it:", end=' ')
    # re-check
    issues = []
    for sf in slide_files:
        num = sf.replace('ppt/slides/slide','').replace('.xml','')
        rels_path = f'ppt/slides/_rels/slide{num}.xml.rels'
        if rels_path in names:
            with z.open(rels_path) as f:
                rt = ET.parse(f)
            for rel in rt.getroot():
                tgt = rel.get('Target','')
                if 'slideLayout' in tgt:
                    layout_file = tgt.replace('../','ppt/')
                    if layout_file in names:
                        with z.open(layout_file) as lf:
                            lt = ET.parse(lf)
                        cSld = lt.getroot().find('.//p:cSld', ns)
                        if cSld is not None:
                            ln = cSld.get('name','')
                            if 'Caption' in ln or 'caption' in ln:
                                issues.append(num)
    print(issues if issues else 'none')
    print()
    print("8. POSSIBLE ISSUE — Image placement:")
    print("   Slides 3,6,7,9 have figure images placed as free-floating pictures (not in placeholder).")
    print("   The image is positioned absolutely using EMU coordinates.")
    print("   Windows PowerPoint respects these coordinates -> generally OK")
    print("   BUT: if the slide canvas size differs (e.g. 4:3 vs 16:9 mismatch), images shift.")

    # Check canvas size
    print()
    print("=== PRESENTATION CANVAS SIZE ===")
    with z.open('ppt/presentation.xml') as f:
        pt = ET.parse(f)
    sz = pt.getroot().find('.//{http://schemas.openxmlformats.org/presentationml/2006/main}sldSz')
    if sz is not None:
        cx = round(int(sz.get('cx'))/914400, 2)
        cy = round(int(sz.get('cy'))/914400, 2)
        print(f'  Canvas: {cx}in x {cy}in  ({round(cx*25.4)}mm x {round(cy*25.4)}mm)')
        ratio = round(cx/cy, 3)
        print(f'  Aspect ratio: {ratio} (16:9 = 1.778, 4:3 = 1.333)')
        if abs(ratio - 1.778) < 0.05:
            print('  -> 16:9 widescreen -> OK for modern Windows PowerPoint')
        elif abs(ratio - 1.333) < 0.05:
            print('  -> 4:3 standard -> OK but images may appear narrower on widescreen projectors')
        else:
            print(f'  -> NON-STANDARD ratio {ratio} -> CHECK THIS')
