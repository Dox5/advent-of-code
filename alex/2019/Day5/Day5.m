clear;
output = 0;
input = 5;
day5 = fopen('input.txt','r');
day5Data = textscan(day5, '%d', 'Delimiter', ',');
day5Data = day5Data{1};
day5Data = transpose(day5Data);
fclose(day5);
i = 1;
print_out = [];
while i ~= 0
    list = str2double(regexp(num2str(day5Data(i)),'\d','match'));
    if day5Data(i) ~= 99
        pos1 = day5Data(i+1)+1;
        pos2 = day5Data(i+2)+1;
        outpos = day5Data(i+3)+1;
    end
    if length(list) > 1 && day5Data(i) ~= 99
        opcode = list(length(list));
        if length(list) == 4
            if list(1) == 1
                second = day5Data(i+2);
            else
                second = day5Data(pos2);
            end
            if list(2) == 1
                first = day5Data(i+1);
            else
                first = day5Data(pos1);
            end
        else
            if list(1) == 1
                first = day5Data(i+1);
            else
                first = day5Data(pos1);
            end
            if opcode < 3 || opcode > 4
                second = day5Data(pos2);
            end
        end    
    elseif day5Data(i) == 99
        output = day5Data(1);
        i = 1;
        break
    else
        opcode = day5Data(i);
        first = day5Data(pos1);
        if opcode < 3 || opcode > 4
            second = day5Data(pos2);
        end
    end
    if opcode == 1
        day5Data(outpos) = first + second;
        i = i + 4;
    elseif opcode == 2
        day5Data(outpos) = first * second;
        i = i + 4;
    elseif opcode == 3
        day5Data(pos1) = input;
        i = i + 2;
    elseif opcode == 4
        print_out = [print_out;day5Data(pos1)];
        i = i + 2;
    elseif opcode == 5
        if first ~= 0
            i = second + 1;
        else
            i = i + 3;
        end
    elseif opcode == 6
        if first == 0
            i = second + 1;
        else
            i = i + 3;
        end
    elseif opcode == 7
        if first < second
            day5Data(outpos) = 1;
        else
            day5Data(outpos) = 0;
        end
        i = i + 4;
    elseif opcode == 8
        if first == second
            day5Data(outpos) = 1;
        else
            day5Data(outpos) = 0;
        end
        i = i + 4;
    end
end


            