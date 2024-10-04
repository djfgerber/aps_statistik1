function Div(el)
  if el.classes[1] == "solution" then
    local summary = pandoc.RawBlock("html", "<details>\n  <summary>Klicke hier, um deine Lösung zu überprüfen.</summary>")
    local endDetails = pandoc.RawBlock("html", "</details>")
    
    table.insert(el.content, 1, summary)
    table.insert(el.content, endDetails)
    
    return el
  end
end