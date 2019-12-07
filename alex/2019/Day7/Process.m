function [output] = Process(data,phase,input)
%PROCESS Summary of this function goes here
%   Detailed explanation goes here
    i = 1;
    tmp = data;
    in_cnt = 0;
    output = input;
    while i ~= 0
        list = str2double(regexp(num2str(tmp(i)),'\d','match'));
        if tmp(i) ~= 99
            pos1 = tmp(i+1)+1;
            pos2 = tmp(i+2)+1;
            outpos = tmp(i+3)+1;
        end
        if length(list) > 1 && tmp(i) ~= 99
            opcode = list(length(list));
            if length(list) == 4
                if list(1) == 1
                    second = tmp(i+2);
                else
                    second = tmp(pos2);
                end
                if list(2) == 1
                    first = tmp(i+1);
                else
                    first = tmp(pos1);
                end
            else
                if list(1) == 1
                    first = tmp(i+1);
                else
                    first = tmp(pos1);
                end
                if opcode < 3 || opcode > 4
                    second = tmp(pos2);
                end
            end    
        elseif tmp(i) == 99
            output = input;
            break
        else
            opcode = tmp(i);
            first = tmp(pos1);
            if opcode < 3 || opcode > 4
                second = tmp(pos2);
            end
        end
        if opcode == 1
            tmp(outpos) = first + second;
            i = i + 4;
        elseif opcode == 2
            tmp(outpos) = first * second;
            i = i + 4;
        elseif opcode == 3
            if in_cnt == 0
                tmp(pos1) = phase;
                in_cnt = 1;
            else
                tmp(pos1) = input;
            end
            i = i + 2;
        elseif opcode == 4
            output = tmp(pos1);
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
                tmp(outpos) = 1;
            else
                tmp(outpos) = 0;
            end
            i = i + 4;
        elseif opcode == 8
            if first == second
                tmp(outpos) = 1;
            else
                tmp(outpos) = 0;
            end
            i = i + 4;
        end
    end
end

