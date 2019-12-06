clear;
day6 = fopen('input.txt', 'r');
day6Data = textscan(day6, '%s', 'Delimiter', '\r\n');
day6Data = day6Data{1};
day6Data = transpose(day6Data);
fclose(day6);
dist = strings(3,2057);
current = 1;
for i = day6Data
    pair = split(i,')');
    if find(strcmp(dist(1,:),pair{1})) >= 1
    else
        dist(1,current) = pair{1};
        current = current + 1;
    end
    if find(strcmp(dist(1,:),pair{2})) >= 1
    else
        dist(1,current) = pair{2};
        current = current + 1;
    end
end
current = 'COM';
count = 0;
%for i = day6Data
COM = find(strcmp(dist(1,:),'COM'));
dist(2,COM) = 0;
dist = MakeTree(day6Data,current,dist);
sum_dist = sum(str2double(dist(2,:)));
list_YOU = [];
current = 'YOU';
while sum(strcmp(dist(1,:),current)) ~= 0
    pos = find(strcmp(dist(1,:),current));
    current = dist(3,pos);
    list_YOU = [list_YOU;current]; %#ok<AGROW>
end
current = 'SAN';
list_SAN = [];
while sum(strcmp(dist(1,:),current)) ~= 0
    pos = find(strcmp(dist(1,:),current));
    current = dist(3,pos);
    list_SAN = [list_SAN;current]; %#ok<AGROW>
end
list_YOU = transpose(list_YOU);
list_SAN = transpose(list_SAN);
lowest = 1000;
for i = list_YOU
    pos = find(strcmp(list_SAN,i));
    if pos >= 1
        if pos < lowest
            lowest = pos;
            meet_at = i;
        end
    end
end
plus = find(strcmp(list_YOU,meet_at));
distance = lowest + plus - 2;