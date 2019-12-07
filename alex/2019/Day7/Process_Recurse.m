function [output, stop_pos, data_state] = Process_Recurse(data,input,config,current,start_pos)
%PROCESS_RECURSE Summary of this function goes here
%   Detailed explanation goes here
    i = start_pos;
    tmp = data;
    in_cnt = start_pos;
    output = input;
    while i ~= 0
        list = str2double(regexp(num2str(tmp(i)),'\d','match'));
        if tmp(i) ~= 99
            if i + 1 < length(tmp)
                pos1 = tmp(i+1)+1;
            end
            if i + 2 < length(tmp)
                pos2 = tmp(i+2)+1;
            end
            if i + 3 < length(tmp)
                outpos = tmp(i+3)+1;
            end
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
            data_state = tmp;
            stop_pos = 0;
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
            if in_cnt == 1
                tmp(pos1) = config(current);
                in_cnt = 2;
            else
                tmp(pos1) = input;
            end
            i = i + 2;
        elseif opcode == 4
            output = tmp(pos1);
            i = i + 2;
            stop_pos = i;
            if tmp(stop_pos) == 99
                stop_pos = 0;
            end
            data_state = tmp;
            break
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

