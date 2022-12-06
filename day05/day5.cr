def part1(input)
  stacks_input, moves_input = input.split("\n\n")
  stacks = build_stacks(stacks_input)
  moves = get_moves(moves_input)

  moves.each do |count, from, to|
    count.times do
      move_once(stacks, from, to)
    end
  end

  get_tops(stacks)
end

def part2(input)
  stacks_input, moves_input = input.split("\n\n")
  stacks = build_stacks(stacks_input)
  moves = get_moves(moves_input)

  moves.each do |count, from, to|
    move_n(stacks, from, to, count)
  end

  get_tops(stacks)
end

def build_stacks(stacks_input)
  stacks = (0...9).map { [] of String }
  lines = stacks_input.lines
  lines[0...(lines.size - 1)].each_with_index do |line, line_idx|
    (0...9).each do |i|
      container_idx = i * 4 + 1
      if line[container_idx] != ' '
        stacks[i].push(line[container_idx].to_s)
      end
    end
  end

  stacks.each do |s|
    s.reverse!
  end

  stacks
end

def get_moves(moves_input)
  re = /move (\d+) from (\d+) to (\d+)/
  moves_input.lines.map do |line|
    count, from, to = re.match(line).not_nil![1..4].map(&.to_i)
    {count, from, to}
  end
end

def print_stacks(stacks)
  max = stacks.map(&.size).max
  (0..(max - 1)).to_a.reverse!.each do |height_idx|
    row = [] of String
    stacks.each do |stack|
      if stack[height_idx]?
        row.push("[#{stack[height_idx]}]")
      else
        row.push("   ")
      end
    end
    puts row.join(" ")
  end
  nil
end

def move_once(stacks, from, to)
  container = stacks[from - 1].pop
  stacks[to - 1].push(container)
end

def move_n(stacks, from, to, n)
  containers = stacks[from - 1].pop(n)
  stacks[to - 1].concat(containers)
end

def get_tops(stacks)
  stacks.map { |s| s.last }.join
end

input = File.read("day5_input.txt")
puts "Part 1: #{part1(input)}"
puts "Part 2: #{part2(input)}"
