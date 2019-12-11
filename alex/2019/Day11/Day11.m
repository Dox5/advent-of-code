clear;
day11Data = readdata('input.txt');
data = day11Data;
start_pos = 1;
base = 0;
map = zeros(50); %21001 for first part
cr = 1; %10501 for first part
cc = 1; %10501 for first part
state = 'N';
done_row = [];
done_col = [];
map(cr,cc) = 1; % this is for second part
while start_pos ~= 0
    if mod(map(cr,cc),2) == 0
        input = 0;
    else
        input = 1;
    end
    [output,start_pos,data,base] = Process(data,input,start_pos,base);
    if isempty(output)
        break
    end
    map(cr,cc) = output;
    done_row = [done_row;cr];
    done_col = [done_col;cc];
    if mod(map(cr,cc),2) == 0
        input = 0;
    else
        input = 1;
    end
    [output,start_pos,data,base] = Process(data,input,start_pos,base);
    if output == 0
        if state == 'N'
            state = 'W';
        elseif state == 'W'
            state = 'S';
        elseif state == 'S'
            state = 'E';
        elseif state == 'E'
            state = 'N';
        end
    elseif output == 1
        if state == 'N'
            state = 'E';
        elseif state == 'W'
            state = 'N';
        elseif state == 'S'
            state = 'W';
        elseif state == 'E'
            state = 'S';
        end
    end
    if state == 'N'
        cr = cr-1;
    elseif state == 'E'
        cc = cc+1;
    elseif state == 'S'
        cr = cr +1;
    elseif state == 'W'
        cc = cc-1;
    end
end
points(:,1) = done_row;
points(:,2) = done_col;
[a,b] = hist3(points, 'Nbins',[200,200]);
num_painted = sum(sum(a>0));
imshow(map)