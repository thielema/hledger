-- Demote all headings one level, for the web manuals (where the page title is the h1).
-- Also, in a manual divided into PARTs (the hledger manual), demote the
-- sections within each part (from a "PART ..." heading to the next one, or to BUGS)
-- one more level, so that the site sidebar shows them nested under their part.
function Pandoc(doc)
  local inpart = false
  for _, b in ipairs(doc.blocks) do
    if b.t == 'Header' then
      if b.level == 1 then
        local s = pandoc.utils.stringify(b.content)
        if s:match('^PART ') then
          inpart = true
          b.level = 0
        elseif s == 'BUGS' then
          inpart = false
        end
      end
      b.level = b.level + (inpart and 2 or 1)
    end
  end
  return doc
end
