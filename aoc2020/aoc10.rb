hasit = Hash.new(0)
numbers = File.read("#{__dir__}/aoc10.in").split("\n").map(&:to_i)
for i in numbers do
  hasit[i] = 1
end
worker = Hash.new{|h,k|
   h[k] = h[k+1]*hasit[k+1] + h[k+2]*hasit[k+2] + h[k+3]*hasit[k+3]
}
worker[numbers.max] = 1
worker[numbers.max + 1] = 0
worker[numbers.max + 2] = 0

puts "#{worker[0]}"
