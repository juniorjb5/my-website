---
# An instance of the Blank widget with a Gallery page element.
# Documentation: https://wowchemy.com/docs/getting-started/page-builder/
widget: blank

# This file represents a page section.
headless: true

active: true

# Order that this section appears on the page. {{< gallery resize_options="50x50" >}}
weight: 95

title: My Gallery

design:
  spacing:
    # Customize the section spacing. Order is top, right, bottom, left.
    padding: ["0px", "0", "0px", "0"]
    
gallery_item:
  image: "1.jpg"
  caption: Write your image 1 caption here

design:
      columns: '1'
---

{{< gallery match="gallery/*" sortOrder="asc" rowHeight="50" margins="2" thumbnailResizeOptions="300x300 q90 Lanczos" showExif=true previewType="blur" embedPreview=true imageResizeOptions="300x300 q90 Lanczos" resize_options="10x10"  >}}


