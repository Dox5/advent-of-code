function [data] = readdata(varargin)
%READDATA Summary of this function goes here
%   Detailed explanation goes here
    file_name = varargin{1};
    file = fopen(file_name,'r');
    scan = textscan(file, '%d64', 'Delimiter', ',');
    fclose(file);
    scan = scan{1};
    if nargin == 2
        num_coppies = varargin{2};
        data_array = zeros(num_coppies,length(scan));
        for i = 1:num_coppies
            data_array(i,:) = transpose(scan);
        end
        data = data_array;
    else
        data = transpose(scan);
    end
end