day1 = fopen('input.txt', 'r');
day1Data = fscanf(day1, '%d');
day1Data1 = transpose(day1Data);
day1Ans = [];
day1Ans2 = [];
for i = day1Data1
    div = floor(i/3);
    day1Ans = [day1Ans;div-2];
end
day1Ans1 = transpose(day1Ans);
for j = day1Ans1
    int = find_fuel(j);
    day1Ans2 = [day1Ans2; int];
end
day1Ans3 = transpose(day1Ans2);
tot1 = sum(day1Ans1);
tot2 = sum(day1Ans2);