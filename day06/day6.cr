def part1(input)
  input.chars
    .each_cons(4)
    .map_with_index(4) { |chars, i| {chars, i} }
    .find! { |chars, i| Set.new(chars).size == 4 }[1]
end

def part2(input)
  input.chars
    .each_cons(14)
    .map_with_index(14) { |chars, i| {chars, i} }
    .find! { |chars, i| Set.new(chars).size == 14 }[1]
end

part1_tests = [
  {"bvwbjplbgvbhsrlpgdmjqwftvncz", 5},
  {"nppdvjthqldpwncqszvftbrmjlhg", 6},
  {"nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10},
  {"zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11},
]

part1_tests.each do |input, expected|
  actual = part1(input)
  if actual != expected
    raise "Part 1 test input: #{input} expected: #{expected} actual: #{actual}"
  end

  puts "Part 1 test input: #{input} expected: #{expected} actual: #{actual}"
end

part2_tests = [
  {"mjqjpqmgbljsphdztnvjfqwrcgsmlb", 19},
  {"bvwbjplbgvbhsrlpgdmjqwftvncz", 23},
  {"nppdvjthqldpwncqszvftbrmjlhg", 23},
  {"nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 29},
  {"zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 26},
]
part2_tests.each do |input, expected|
  actual = part2(input)
  if actual != expected
    raise "Part 2 test input: #{input} expected: #{expected} actual: #{actual}"
  end

  puts "Part 2 test input: #{input} expected: #{expected} actual: #{actual}"
end

input = File.read("day6_input.txt")
puts "Part 1: #{part1(input)}"
puts "Part 2: #{part2(input)}"
