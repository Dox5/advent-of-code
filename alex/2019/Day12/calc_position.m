function [position] = calc_position(data)
%CALC_POSITION Summary of this function goes here
%   Detailed explanation goes here
    data_size = size(data);
    for i = 1:data_size(1)
        for j = 1:data_size(2)
            data(i,j,1) = data(i,j,1) + data(i,j,2);
        end
    end
    position = data(:,:,1);
end

