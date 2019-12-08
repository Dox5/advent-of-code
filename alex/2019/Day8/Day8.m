clear;
day8 = fopen('input.txt','r');
day8Data = textscan(day8, '%1d');
fclose(day8);
day8Data = day8Data{1};
day8Data = transpose(day8Data);
width = 25;
height = 6;
layers = length(day8Data)/(width*height);
layout = zeros(height,width,layers);
position = 1;
for k = 1:layers
    for j = 1:height
        for i = 1:width
            layout(j,i,k) = day8Data(position);
            position = position + 1;
        end
    end
end
least_num = 15000;
for l = 1:layers
    num = sum(sum(layout(:,:,l)==0));
    if num < least_num
        least_num = num;
        best_layer = l;
    end
end
num1 = sum(sum(layout(:,:,best_layer)==1));
num2 = sum(sum(layout(:,:,best_layer)==2));
prod = num1 * num2;

image = zeros(height,width);
for i = 1:width
    for j = 1:height
        for k = 1:layers
            if layout(j,i,k) ~= 2
                image(j,i) = layout(j,i,k);
                break
            end
        end
    end
end
imshow(image);