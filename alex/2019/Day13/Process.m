function [output, result] = Process(data,input_v)
%PROCESS Summary of this function goes here
%   Detailed explanation goes here
    i = 1;
    tmp = cast(data,'int64');
    output = [];
    base = 1;
    draw_now = 0;
    while i ~= 0
        if find(mod(find(output == 4),3)==0) > 0
            draw_now = 1;
        else
            draw_now = 0;
        end

        list = str2double(regexp(num2str(tmp(i)),'\d','match'));
        if tmp(i) ~= 99
            pos1 = tmp(i+1)+1;
            pos2 = tmp(i+2)+1;
            if i + 3 <= length(tmp)
                outpos = tmp(i+3)+1;
            else
                outpos = 1;
            end
        end
        if length(list) > 1 && tmp(i) ~= 99
            opcode = list(length(list));
            if length(list) == 5
                if list(1) == 1
                    error('No immidiate output');
                elseif list(1) == 0
                    outpos = outpos;
                else
                    outpos = outpos + base;
                end
                if list(2) == 1
                    if i + 2 <= length(tmp)
                        second = tmp(i+2);
                    else
                        second = 0;
                    end
                elseif list(2) == 0
                    if pos2 <= length(tmp)
                        second = tmp(pos2);
                    else
                        second = 0;
                    end
                else
                    if base + pos2 <= length(tmp)
                        second = tmp(base + pos2);
                    else
                        second = 0;
                    end
                end
                if list(3) == 1
                    if i + 1 <= length(tmp)
                        first = tmp(i+1);
                    else
                        first = 0;
                    end
                elseif list(3) == 0
                    if pos1 <= length(tmp)
                        first = tmp(pos1);
                    else
                        first = 0;
                    end
                else
                    if base + pos1 <= length(tmp)
                        first = tmp(base + pos1);
                    else
                        first = 0;
                    end
                end
            elseif length(list) == 4
                if list(1) == 1
                    if i + 2 <= length(tmp)
                        second = tmp(i+2);
                    else
                        second = 0;
                    end
                elseif list(1) == 0
                    if pos2 <= length(tmp)
                        second = tmp(pos2);
                    else
                        second = 0;
                    end
                else
                    if base + pos2 <= length(tmp)
                        second = tmp(base + pos2);
                    else
                        second = 0;
                    end
                end
                if list(2) == 1
                    if i + 1 <= length(tmp)
                        first = tmp(i+1);
                    else
                        first = 0;
                    end
                elseif list(2) == 0
                    if pos1 <= length(tmp)
                        first = tmp(pos1);
                    else
                        first = 0;
                    end
                else
                    if base + pos1 <= length(tmp)
                        first = tmp(base + pos1);
                    else
                        first = 0;
                    end
                end
            else
                if list(1) == 1
                    if i + 1 <= length(tmp)
                        first = tmp(i+1);
                    else
                        first = 0;
                    end
                elseif list(1) == 0
                    if pos1 <= length(tmp)
                        first = tmp(pos1);
                    else
                        first = 0;
                    end
                else
                    if base + pos1 <= length(tmp)
                        first = tmp(base + pos1);
                    else
                        first = 0;
                    end
                end
                if opcode < 3 || (4 < opcode && opcode < 9)
                    if pos2 <= length(tmp)
                        second = tmp(pos2);
                    else
                        second = 0;
                    end
                end
            end    
        elseif tmp(i) == 99
            i = i + 1;
            stop_pos = 0;
            data_state = tmp;
            base_state = base;
            break
        else
            opcode = tmp(i);
            if pos1 <= length(tmp)
                first = tmp(pos1);
            else
                first = 0;
            end
            if opcode < 3 || (4 < opcode && opcode < 9)
                if pos2 <= length(tmp)
                    second = tmp(pos2);
                else
                    second = 0;
                end
            end
        end
        if tmp(i) ~= 99
            if opcode == 1
                tmp(outpos) = first + second;
                i = i + 4;
            elseif opcode == 2
                tmp(outpos) = first * second;
                i = i + 4;
            elseif opcode == 3
                if mod(length(output),3) == 0 && draw_now == 1
                    [map, result] = draw(output);
                    imshow(map,[]);
                    truesize([400 400]);
                    drawnow()
                end
                [~, col3] = find(map == 3);
                [~, col4] = find(map == 4);
                if col4 > col3
                    input_v = 1;
                elseif col4 < col3
                    input_v = -1;
                else
                    input_v = 0;
                end
                if list(1) == 2
%                     try
%                         input_v = input('Joystick pos');
%                     catch
%                         input_v = input('Joystick pos');
%                     end
                    tmp(pos1 + base) = input_v;
                elseif list(1) == 1
                    error('Dont expect this');
                else
%                     try
%                         input_v = input('Joystick pos');
%                     catch
%                         input_v = input('Joystick pos');
%                     end
                    tmp(pos1) = input_v;
                end
                i = i + 2;
            elseif opcode == 4
                output = [output;first];
                i = i + 2;
                stop_pos = i;
                if tmp(stop_pos) == 99
                    stop_pos = 0;
                end
                data_state = tmp;
                base_state = base;
                %break
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
            elseif opcode == 9
                base = base + first;
                i = i + 2;
            else
                error('Invalid Opcode');
            end
        end
    end
end