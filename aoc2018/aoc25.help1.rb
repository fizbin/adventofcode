cons = []
File.readlines('f').map do |line|
    x,y,z,w = line.split(',').map(&:to_i)
    joined = []
    cons.each.with_index do |a,i|
        if a.map{|xx,yy,zz,ww| (x-xx).abs + (y-yy).abs + (z-zz).abs + (w-ww).abs <= 3}.any?
            a.push [x,y,z,w]
            joined.push i
        end
    end
    if joined.empty?
        cons.push [[x,y,z,w]]
    else
        newcons = joined.sort.reverse.flat_map{|i|cons.delete_at i}
        cons.push newcons + [[x,y,z,w]]
    end
end
p cons.size

