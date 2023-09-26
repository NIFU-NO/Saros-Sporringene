-- remove_empty_headings.lua
local heading_stack = {}

function Header(el)
    -- Check if the current heading is empty
    local next_el = pandoc.utils.peek_next()
    local is_empty = next_el == nil or (next_el.t == "Header" and next_el.level <= el.level)
    
    -- Check and mark parent headings as empty if all their child headings are empty
    for i = #heading_stack, 1, -1 do
        if heading_stack[i].level < el.level then
            break -- Stop when a parent heading is reached
        elseif heading_stack[i].is_empty then
            if i > 1 and heading_stack[i - 1].level < heading_stack[i].level then
                heading_stack[i - 1].is_empty = true -- Mark the parent heading as empty
            end
        else
            break -- Stop if a non-empty heading is encountered
        end
    end
    
    -- Remove empty headings from the stack
    local removed_elements = {}
    while #heading_stack > 0 and heading_stack[#heading_stack].is_empty do
        table.insert(removed_elements, pandoc.Null()) -- Remove the empty heading
        table.remove(heading_stack)
    end
    
    -- Add the current heading to the stack
    table.insert(heading_stack, { level = el.level, is_empty = is_empty })
    
    -- Return the removed elements followed by the current heading
    table.insert(removed_elements, el)
    return removed_elements
end

function Pandoc(doc)
    -- Remove remaining empty headings from the stack
    local removed_elements = {}
    while #heading_stack > 0 and heading_stack[#heading_stack].is_empty do
        table.insert(removed_elements, pandoc.Null()) -- Remove the empty heading
        table.remove(heading_stack)
    end
    
    -- Append the removed elements to the end of the document
    for _, el in ipairs(removed_elements) do
        table.insert(doc.blocks, el)
    end
    
    return doc
end
