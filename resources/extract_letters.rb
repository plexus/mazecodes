W = 9
H = 8

(0..35).each do |i|
  if i < 26
    letter = (97+i).chr
  else
    letter = (48+i-26).chr
  end
  x = i % 16
  offset_x = x*W + (x > 3 ? 10 : 0) + (x > 7 ? 10 : 0) + (x > 11 ? 9 : 0)
  offset_y = (i/16)*16
  puts "convert letters_orig.png -crop #{W}x#{H}+#{offset_x}+#{offset_y} letters/#{letter}.png"
end
