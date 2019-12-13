clear;
day12 = fopen('input.txt','r');
day12Data = textscan(day12, '%s', 'Delimiter','>');
splitData = regexp(day12Data{1},'[-]?\d+','match');
sim_steps = 1000;
sim_done = 0;
for i = 1:length(splitData)
    data(i,:) = splitData{i};
end
data = str2double(data);
data(:,:,2) = zeros(size(data));
data_size = size(data);
for i = 1:sim_steps
    data(:,:,2) = calc_velocity(data);
    data(:,:,1) = calc_position(data);
end
energy = calc_energy(data);
for i = 1:length(splitData)
    data(i,:,1) = str2double(splitData{i});
end
data(:,:,2) = zeros(data_size(1),data_size(2));
data_history = data;
iteration = 0;
first_done = 0;
second_done = 0;
third_done = 0;
fourth_done = 0;
while sim_done == 0
    data(:,:,2) = calc_velocity(data);
    data(:,:,1) = calc_position(data);
    iteration = iteration + 1;
    if (data_history(:,1,2) == data(:,1,2)) & (first_done == 0)
        first_done = iteration;
    end
    if data_history(:,2,2) == data(:,2,2) & (second_done == 0)
        second_done = iteration;
    end
    if data_history(:,3,2) == data(:,3,2) & (third_done == 0)
        third_done = iteration;
    end
%     if data_history(4,:,1) == data(4,:,1) & (fourth_done == 0)
%         fourth_done = iteration;
%     end
    if (first_done ~= 0) && (second_done ~= 0) && (third_done ~= 0) %&& (fourth_done ~= 0)
        sim_done = 1;
    end
    
end
matrix = sym([first_done second_done third_done]);
format long;
lcm(matrix)*2