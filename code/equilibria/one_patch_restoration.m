clear  % Clears command window
clc    % Clears command history
clf    % Removes anything in the figure window before simulation.

%%%%%% define the variables here
r = 0.55;
r_vec = repmat(0.55, [20,3]);
r_vec = r_vec(:);
a = [0.1 0.3 0.5];
a_vec = repmat([0.1 0.3 0.5], [20 1]);
a_vec = a_vec(:); 
d = 0.24;
d_vec = repmat(0.24, [20 3]);
d_vec = d_vec(:);
y = 0.77;
y_vec = repmat(0.77, [20 3]);
y_vec = y_vec(:);
g = 0.3;
g_vec = repmat(0.3, [20 3]);
g_vec = g_vec(:);
z = 0;
z_vec = repmat(0, [20 3]); %#ok<REPMAT>
z_vec = z_vec(:);

%make a vector for the number of the equilibrium values                              
eqi_num = repmat(1:20, [1 3]); %this is a way to make a vector of 
                                %proper length
eqi_num = eqi_num(:);

                              
%%%%%% define the matrix for the data
data = 2.*ones(60,6); %rows, columns -- filling them with 2s
%2 columns for eigenvalues and equi values each, and 3 columns for 
%so 7 columns
data(1:end,1) = r_vec;
data(1:end,2) = a_vec; 
data(1:end,3) = d_vec; 
data(1:end,4) = y_vec;
data(1:end,5) = g_vec;
data(1:end,6) = z_vec;
data(2:end,7) = eqi_num;


%%%%%% loop through the combinations
for i = 0:3
syms c m %state variables - x is C, y is M

%equations: dC/dt = rCT + zrT - dC - aMC, dM/dt = aMC - (gM)/(T+M) + yMT
%define the equations - all the data(...) are calling from the named data
%vectors above
eqns = [data((i*20)+1,1)*c*(1-m-c) + ...
    data((i*20)+1,6)*data((i*20)+1,1)*(1-c-m) - ...
    data((i*20)+1,3)*c - data((i*20)+1,2)*m*c  == 0, ...
    data((i*20)+1,2)*m*c - ...
    (data((i*20)+1,5)*m)/((1-c-m)+m)...
    + data((i*20)+1,4)*m*(1-c-m) == 0];

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
    data((i*20)+1,3)*c - data((i*20)+1,2)*m*c, ...
    data((i*20)+1,2)*m*c - ...
    (data((i*20)+1,5)*m)/((1-c-m)+m)...
    + data((i*20)+1,4)*m*(1-c-m)];

J = jacobian(eqns_2, [c,m]); %solves the Jacobian
for j = 1:length(S.c)
    c = S.c(j);
    m = S.m(j);
    Jval = subs(J); %subs in the values of the variables above into J
    e_Jval = eig(Jval);
    data((i*20)+j,10) = e_Jval(1);
    data((i*20)+j,11) = e_Jval(2);
end

end

%let's try to get the analytical solutions - 'unable to solve symbolically'

% NOTE - not sure what the p_c q_c variables are for? 
syms c m r a d y g z 
assume (0 < c < 1 & 0 < m < 1)
eqns_3 = [r*c*(1-m-c) + ...
    z*r*(1-c-m) - ...
    d*c - ...
    a*m*c, ...
    a*m*c - ...
    (g*m)/((1-c-m)+m) + ...
    y*m*(1-c-m)];
B = solve(eqns_3, syms);

%filename = "equilibria_eigenvalues.xlsx"
%the code below failed to 'start the Excel server' so it made a bunch of
%.csv files...hence why I gave them different names bc only 1 sheet
xlswrite("twopatch_fullsolve.xlsx",data,'All data','A1')

