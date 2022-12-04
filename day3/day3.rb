require "set"

class Day3
  PRIORITY = [nil] + ("a".."z").to_a + ("A".."Z").to_a

  def part1(input)
    result = input.split("\n").map { |s| PRIORITY.index(common(s)) }.sum
    puts("Part 1: #{result}")
  end

  def part2(input)
    result = input
      .split("\n")
      .each_slice(3)
      .map { |arr| arr.map { |s| Set.new(s.split("")) }.reduce(&:&).to_a[0] }
      .map { |s| PRIORITY.index(s) }
      .sum
    puts("Part 2: #{result}")
  end

  def common(s)
    s1 = s[..(s.length / 2 - 1)]
    s2 = s[(s.length / 2)..]
    (Set.new(s1.split("")) & Set.new(s2.split(""))).to_a[0]
  end
end

input = File.open("day3_input.txt", "r") { |f| f.read }
Day3.new.part1(input)
Day3.new.part2(input)
