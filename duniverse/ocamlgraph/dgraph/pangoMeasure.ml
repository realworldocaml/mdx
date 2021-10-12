let withContext ~context ~fontName ~fontSize text =
  let font_description = Pango.Font.from_string fontName in
  Pango.Font.modify font_description
    ~size:(fontSize * Pango.scale)
    ();
  Pango.Context.set_font_description context font_description;
  let layout = Pango.Layout.create context in
  Pango.Layout.set_text layout text;
  Pango.Layout.get_pixel_size layout
