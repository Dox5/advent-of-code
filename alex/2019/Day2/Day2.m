clear;
output = 0;
while output ~= 19690720
    for noun = 0:99
        for verb = 0:99
            day2 = fopen('input.txt','r');
            day2Data = textscan(day2, '%d', 'Delimiter', ',');
            day2Data = day2Data{1};
            day2Data = transpose(day2Data);
            fclose(day2);
            day2Data(2) = noun;
            day2Data(3) = verb;
            for i = 1:4:149
                pos1 = day2Data(i+1)+1;
                pos2 = day2Data(i+2)+1;
                outpos = day2Data(i+3)+1;
                if day2Data(i) == 1
                    day2Data(outpos) = day2Data(pos1) + day2Data(pos2);
                elseif day2Data(i) == 2
                    day2Data(outpos) = day2Data(pos1) * day2Data(pos2);
                elseif day2Data(i) == 99
                    output = day2Data(1);
                    break
                end
                if output == 19690720
                    break
                end
            end
            if output == 19690720
               break
            end
        end
        if output == 19690720
            break
        end
    end
    if output == 19690720
        break
    end
end


            