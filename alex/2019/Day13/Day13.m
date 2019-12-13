clear;
day13Data = readdata('input.txt');
day13Data(1) = 2;
figure
[output, result] = Process(day13Data);
map = draw(output);
blocks = sum(sum(map == 2));