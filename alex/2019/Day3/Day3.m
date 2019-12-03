day3 = fopen('input.txt','r');
day3Data = textscan(day3, '%s', 'Delimiter', ',');
fclose(day3);
day3Data = day3Data{1};
day3Data = transpose(day3Data);
L1 = day3Data(1:301);
L2 = day3Data(302 : end);
map = zeros(20001,20001,3);
wr = 10001;
wc = 10001;
map(wr,wc,1) = 1;
for i = L1
    up = strfind(i, 'U');
    down = strfind(i, 'D');
    left = strfind(i, 'L');
    right = strfind(i, 'R');
    val = sscanf(i{1}(2:end),'%d',1);
    for j = 1:val
        if up{1} == 1
            wr = wr - 1;
            map(wr,wc,2) = map(wr + 1,wc,2) + 1;
        elseif down{1} == 1
            wr = wr + 1;
            map(wr,wc,2) = map(wr - 1,wc,2) + 1;
        elseif left{1} == 1
            wc = wc - 1;
            map(wr,wc,2) = map(wr,wc + 1,2) + 1;
        elseif right{1} == 1
            wc = wc + 1;
            map(wr,wc,2) = map(wr,wc - 1,2) + 1;
        end
        map(wr,wc,1) = 2;
    end
end
wr = 10001;
wc = 10001;
for i = L2
    up = strfind(i, 'U');
    down = strfind(i, 'D');
    left = strfind(i, 'L');
    right = strfind(i, 'R');
    val = sscanf(i{1}(2:end),'%d',1);
    for j = 1:val
        if up{1} == 1
            wr = wr - 1;
            map(wr,wc,3) = map(wr + 1,wc,3) + 1;
        elseif down{1} == 1
            wr = wr + 1;
            map(wr,wc,3) = map(wr - 1,wc,3) + 1;
        elseif left{1} == 1
            wc = wc - 1;
            map(wr,wc,3) = map(wr,wc + 1,3) + 1;
        elseif right{1} == 1
            wc = wc + 1;
            map(wr,wc,3) = map(wr,wc - 1,3) + 1;
        end
        if map(wr,wc) == 2
            map(wr,wc) = 4;
        elseif map(wr,wc) == 0
            map(wr,wc) = 3;
        end
    end
end
[row, column] = find(map(:,:,1) == 4);
dis_tot = 0;
for i = 1:length(row)
    dis_tot = [dis_tot;map(row(i),column(i),2) + map(row(i),column(i),3)];
end
row = row - 10001;
column = column - 10001;
dist = abs(row) + abs(column);
[min_val, ~] = min(dist);
[min_tot, ~] = min(dis_tot(2:end));