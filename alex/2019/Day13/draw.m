function [map, result] = draw(data)
%DRAW Summary of this function goes here
%   Detailed explanation goes here
    tmp_map = zeros(50);
    i = 1;
    while i < length(data)
        if data(i+1) == 0 && data(i) == -1
            result = data(i+2);
        else
            tmp_map(data(i+1)+1,data(i)+1) = data(i+2);
        end
        i = i + 3;
    end
    map = tmp_map;
end

