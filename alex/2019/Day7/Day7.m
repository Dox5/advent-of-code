clear;
best_out = 0;
day7 = fopen('input.txt','r');
day7Data = textscan(day7, '%d', 'Delimiter', ',');
day7Data = day7Data{1};
day7Data = transpose(day7Data);
fclose(day7);
mode = 1;
a_pos = 1;
b_pos = 1;
c_pos = 1;
d_pos = 1;
e_pos = 1;
oute = 0;
a_data = day7Data;
b_data = day7Data;
c_data = day7Data;
d_data = day7Data;
e_data = day7Data;
for a = 5:9
    for b = 5:9
        for c = 5:9
            for d = 5:9
                for e = 5:9
                    config = [a;b;c;d;e];
                    if sum(sum(config==config')) == 5
                        if mode == 0
                            outa = Process(day7Data,a,0);
                            outb = Process(day7Data,b,outa);
                            outc = Process(day7Data,c,outb);
                            outd = Process(day7Data,d,outc);
                            output = Process(day7Data,e,outd);
                        else
                            while e_pos ~= 0
                                [outa, a_pos, a_data] = Process_Recurse(a_data,oute,config,1,a_pos);
                                [outb, b_pos, b_data] = Process_Recurse(b_data,outa,config,2,b_pos);
                                [outc, c_pos, c_data] = Process_Recurse(c_data,outb,config,3,c_pos);
                                [outd, d_pos, d_data] = Process_Recurse(d_data,outc,config,4,d_pos);
                                [oute, e_pos, e_data] = Process_Recurse(e_data,outd,config,5,e_pos);
                            end
                            output = oute;
                        end
                        if output > best_out
                            best_out = output;
                            best_config = [a;b;c;d;e];
                        end
                    end
                    a_pos = 1;
                    b_pos = 1;
                    c_pos = 1;
                    d_pos = 1;
                    e_pos = 1;
                    oute = 0;
                    a_data = day7Data;
                    b_data = day7Data;
                    c_data = day7Data;
                    d_data = day7Data;
                    e_data = day7Data;
                end
            end
        end
    end
end
