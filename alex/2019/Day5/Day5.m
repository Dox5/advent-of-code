clear;
output = 0;
input = 5;
day2 = fopen('input.txt','r');
day2Data = textscan(day2, '%d', 'Delimiter', ',');
day2Data = day2Data{1};
day2Data = transpose(day2Data);
fclose(day2);
i = 1;
print_out = [];
while i ~= 0
    list = str2double(regexp(num2str(day2Data(i)),'\d','match'));
    if day2Data(i) ~= 99
        pos1 = day2Data(i+1)+1;
        pos2 = day2Data(i+2)+1;
        outpos = day2Data(i+3)+1;
    end
    if length(list) > 1 && day2Data(i) ~= 99
        opcode = list(length(list));
        if length(list) == 4
            if list(1) == 1
                second = day2Data(i+2);
            else
                second = day2Data(pos2);
            end
            if list(2) == 1
                first = day2Data(i+1);
            else
                first = day2Data(pos1);
            end
        else
            if list(1) == 1
                first = day2Data(i+1);
            else
                first = day2Data(pos1);
            end
            if opcode < 3 || opcode > 4
                second = day2Data(pos2);
            end
        end    
    elseif day2Data(i) == 99
        output = day2Data(1);
        i = 1;
        break
    else
        opcode = day2Data(i);
        first = day2Data(pos1);
        if opcode < 3 || opcode > 4
            second = day2Data(pos2);
        end
    end
    if opcode == 1
        day2Data(outpos) = first + second;
        i = i + 4;
    elseif opcode == 2
        day2Data(outpos) = first * second;
        i = i + 4;
    elseif opcode == 3
        day2Data(pos1) = input;
        i = i + 2;
    elseif opcode == 4
        print_out = [print_out;day2Data(pos1)];
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
            day2Data(outpos) = 1;
        else
            day2Data(outpos) = 0;
        end
        i = i + 4;
    elseif opcode == 8
        if first == second
            day2Data(outpos) = 1;
        else
            day2Data(outpos) = 0;
        end
        i = i + 4;
    end
end


            