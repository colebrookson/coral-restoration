clear  % Clears command window
clc    % Clears command history
clf    % Removes anything in the figure window before simulation.

data = readmatrix('C:\Users\brookson\Documents\Github\coral-restoration\data\parameter-data\full-params-for-matlab.csv');

%%%%%% loop through the combinations
for i = 0:39999
    
syms c m %state variables 

%equations: dC/dt = rCT + zrT - dC - aMC, dM/dt = aMC - (gM)/(T+M) + yMT
%define the equations - all the data(...) are calling from the named data
%vectors above
eqns = [data((i*20)+1,1)*c*(1-m-c) + ...
    data((i*20)+1,6)*data((i*20)+1,1)*(1-c-m) - ...
    data((i*20)+1,2)*c - data((i*20)+1,4)*m*c  == 0, ...
    data((i*20)+1,4)*m*c - ...
    (data((i*20)+1,5)*m)/((1-c-m)+m)...
    + data((i*20)+1,3)*m*(1-c-m) == 0];

%make an initial guess which bounds the state parameters so the search
%space is now bounded
init_guess = [0 1; 0 1];

%now use the vpasolve (numerical solver) with the state variables, the eqns
%and the initial boundings
S = vpasolve(eqns, [c,m], init_guess);  

%let c1 = x, m1 = y, c2 = w, m2 = v
%Xequi(1:length(S.x),i) = S.x;
len = 20 - length(S.c);
data((i*20)+1:((i+1)*20)-len,8) = S.c; %how matlab formats outputs of vpasolve
data((i*20)+1:((i+1)*20)-len,9) = S.m;

%Now solve for the jacobian. Here, these equations are the same as above, 
%but this time we're solving symbolically and looking at the Jacobian, 
%so therefore not set equal to zero
eqns_2 = [data((i*20)+1,1)*c*(1-m-c) + ...
    data((i*20)+1,6)*data((i*20)+1,1)*(1-c-m) - ...
    data((i*20)+1,2)*c - data((i*20)+1,4)*m*c, ...
    data((i*20)+1,4)*m*c - ...
    (data((i*20)+1,5)*m)/((1-c-m)+m)...
    + data((i*20)+1,3)*m*(1-c-m)];

J = jacobian(eqns_2, [c,m]); %solves the Jacobian
for j = 1:length(S.c)
    c = S.c(j);
    m = S.m(j);
    Jval = subs(J); %subs in the values of the variables above into J
    e_Jval = eig(Jval);
    data((i*20)+j,10) = e_Jval(1);
    data((i*20)+j,11) = e_Jval(2);
end

disp(i)

end

%let's try to get the analytical solutions
%NOTE - wondering if this should happen in another script if we think it 
%may take some time? 
%syms c m r a d y g z 
%assume (0 < c < 1 & 0 < m < 1)
%eqns_3 = [r*c*(1-m-c) + ...
%    z*r*(1-c-m) - ...
%    d*c - ...
 %   a*m*c, ...
%    a*m*c - ...
%    (g*m)/((1-c-m)+m) + ...
%    y*m*(1-c-m)];
%B = solve(eqns_3, syms);

%unsure if this is the best way to write these files? 
%will look into it. 

%filename = "equilibria_eigenvalues.xlsx"
%the code below failed to 'start the Excel server' so it made a bunch of
%.csv files...hence why I gave them different names bc only 1 sheet
xlswrite("full_restoration_model_output.xlsx",data,'All data','A1');
writematrix(data, "C:/Users/brookson/Documents/GitHub/coral-restoration/data/parameter-data/full-restoration-model-output.txt");


