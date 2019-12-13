function [velocity] = calc_velocity(data)
%CALC_VELOCITY Summary of this function goes here
%   Detailed explanation goes here
  data_size = size(data);
  for i = 1:data_size(1)
      for j = 1:data_size(2)
          for k = 1:data_size(1)
              if data(i,j,1) < data(k,j,1)
                  data(i,j,2) = data(i,j,2) + 1;
              elseif data(i,j,1) > data(k,j,1)
                  data(i,j,2) = data(i,j,2) - 1;
              end
          end
      end
  end
  velocity = data(:,:,2);
end

