module Day7
  class File
    getter name : String
    getter size : Int32

    def initialize(name : String, size : Int32)
      @name = name
      @size = size
    end
  end

  class Directory
    getter name : String
    getter files : Array(File)
    getter directories : Array(Directory)

    def self.print(directory : Directory, indent = 0)
      puts " " * indent + directory.name + "/"
      directory.files.each do |f|
        puts " " * (indent + 2) + "#{f.name} (#{f.size})"
      end
      directory.directories.each do |d|
        Directory.print(d, indent + 2)
      end
    end

    def initialize(name : String, files : Array(File), directories : Array(Directory))
      @name = name
      @files = files
      @directories = directories
    end

    def size : Int32
      files.map(&.size).sum + directories.map(&.size).sum
    end

    def add_directory(directory)
      @directories.push(directory)
    end

    def add_file(file)
      @files.push(file)
    end
  end

  def self.test
    root = Directory.new(
      "",
      [
        File.new("a", 10),
        File.new("b", 20),
      ], [
      Directory.new(
        "x",
        [File.new("c", 30)],
        [] of Directory,
      ),
    ],
    )
    Directory.print(root)
    puts "total: #{root.size}"
  end

  def self.build_tree(input : String) : Directory
    lines = input.lines
    if lines[0] != "$ cd /"
      raise "Unexpected first line: #{lines[0]}"
    end

    lines = lines.map { |line| line.split(" ") }
    path = [Directory.new("", [] of File, [] of Directory)]
    idx = 1
    while idx < lines.size
      words = lines[idx]

      if words[0] == "$"
        cmd = words[1]
        case cmd
        when "cd"
          dirname = words[2]
          if dirname == ".."
            path.pop
          else
            current_dir = Directory.new(dirname, [] of File, [] of Directory)
            if !path.empty?
              path[-1].add_directory(current_dir)
            end
            path.push(current_dir)
          end
        when "ls"
          # $ ls
          # dir cjdhwbv
          # 268173 lsmmthf
          # 99445 vwl.vsq
          dir = path[-1]
          idx += 1
          while idx < lines.size && lines[idx][0] != "$"
            if lines[idx][0] == "dir"
              # skip dir entry in ls output. directory will be created during cd.
            else
              size = lines[idx][0].to_i
              name = lines[idx][1]
              dir.add_file(File.new(name, size))
            end

            idx += 1
          end

          next
        else
          raise "Unexpected command: #{cmd}"
        end
      end

      idx += 1
    end

    path[0]
  end

  def self.collect_directories(dir)
    result = [dir]
    dir.directories.each do |d|
      result.concat(collect_directories(d))
    end
    result
  end

  def self.part1(input) : Int32
    root = build_tree(input)
    collect_directories(root).map(&.size).select { |size| size <= 100000 }.sum
  end

  def self.part2(input) : Int32
    available = 70000000
    unused_required = 30000000
    root = build_tree(input)
    need_to_delete = unused_required - (available - root.size)
    collect_directories(root).select { |d| d.size >= need_to_delete }.map(&.size).min
  end
end

# Day7.test

test_input = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"

expected1 = 95437
actual1 = Day7.part1(test_input)
if actual1 != expected1
  raise "test 1 failed: expected #{expected1}, actual #{actual1}: #{expected1 == actual1}"
end

expected2 = 24933642
actual2 = Day7.part2(test_input)
if actual2 != expected2
  raise "test 2 failed: expected #{expected2}, actual #{actual2}"
end

input = File.read("day7_input.txt")
puts "Part 1: #{Day7.part1(input)}"
puts "Part 2: #{Day7.part2(input)}"
