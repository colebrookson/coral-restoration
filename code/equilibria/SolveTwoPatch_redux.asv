clear  % Clears command window
clc    % Clears command history
clf    % Removes anything in the figure window before simulation.

r = 0.55
a = 0.1
d = 0.24
gamma = 0.77
epsilon = [0.005 0.01 0.05 0.1 0.5]
zeta = [0.005 0.01 0.05 0.1 0.5]
g_vec = 0.01:0.01:0.99 %first, step, last
%(rows, columns)

%%%% So if there needs to be 20 



data = 2.*ones(9900,12) %see 11.9.2018 notes
%column 1 = g, column 2,3,4,5 = recruitment values, column 6 = equi (1 - 20
%repeated), column 7,8,9,10 = equi values, column 11,12,13,14 =
%eigenvalues



recr_vec = repmat([0.1 0.3 0.5], [20 99]) %20 each element, repeat whole thing 99 times
recr_vec = recr_vec(:)%recr_vec is recruitment aka dispersal
equinum = repmat(1:20, [1 495])
equinum = equinum(:)

data(1:end,1) = gvecvec %column 1 = r
data(1:end,2) = recr_vec % y (gamma)
data(1:end,3) = recr_vec % d
data(1:end,4) = equinum % a
data(1:end,5) = gvecvec % g
data(1:end,6) = recr_vec % z


for i = 0:495
syms x y %state variables - x is C, y is M
eqns = [data((i*20)+1,1)*y*x + data((i*20)+1,1)*data((i*20)+1,6)*y - data((i*20)+1,3)*x - data((i*20)+1,4)*x*y  == 0, data((i*20)+1,4)*x*y - (data((i*20)+1,5)*y)/(1-x) + data((i*20)+1,2)*y*(1-x) == 0];
init_guess = [0 1; 0 1; 0 1; 0 1];%init guess says that the state variable 
%are bounded between 0 and 1 so it bounds the search space
S = vpasolve(eqns, [x,y,w,v], init_guess) %some sol'ns with numerical instabilities but those are all ones outside the specified range

%let c1 = x, m1 = y, c2 = w, m2 = v
%Xequi(1:length(S.x),i) = S.x;
len = 20 - length(S.x);
data((i*20)+1:((i+1)*20)-len,5) = S.x; %how matlab formats outputs of vpasolve
data((i*20)+1:((i+1)*20)-len,6) = S.y;
data((i*20)+1:((i+1)*20)-len,7) = S.w;
data((i*20)+1:((i+1)*20)-len,8) = S.v;


eqns2 = [r*(1-x-y)*x - d*x - a*x*y + data((i*20)+1,3)*w*(1-x-y), a*x*y - (data((i*20)+1,1)*y)/(1-x) + gamma*y*(1-x-y) + data((i*20)+1,2)*v*(1-x-y), r*(1-w-v)*w - d*w - a*w*v + data((i*20)+1,3)*x*(1-w-v), a*w*v - (data((i*20)+1,1)*v)/(1-w) + gamma*v*(1-w-v) + data((i*20)+1,2)*y*(1-w-v)];
%here, these equations are the same as above, but this time we're solving
%symbolically and looking at the Jacobian, so therefore not set equal to
%zero
J = jacobian(eqns2, [x,y,w,v]) %solves the Jacobian
for j = 1:length(S.x)
    x = S.x(j);
    y = S.y(j);
    w = S.w(j);
    v = S.v(j);
    Jval = subs(J); %subs in the values of the variables above into J
    e_Jval = eig(Jval);
    data((i*20)+j,9) = e_Jval(1);
    data((i*20)+j,10) = e_Jval(2);
    data((i*20)+j,11) = e_Jval(3);
    data((i*20)+j,12) = e_Jval(4);
end

end

%let's try to get the analytical solutions - 'unable to solve symbolically'

syms x y w v r a d gamma zeta g p_c q_c p_m q_m
assume (0 < x < 1 & 0 < y < 1 & 0 < w < 1 & 0 < v < 1)
eqns2 = [r*(1-q_c)*(1-x-y)*x - d*x - a*x*y + p_c*r*w*(1-x-y), a*x*y - (g*y)/(1-x) + gamma*(1-q_m)*y*(1-x-y) + gamma*p_m*v*(1-x-y), r*(1-p_c)*(1-w-v)*w - d*w - a*w*v + q_c*r*x(1-w-v), a*w*v - (g*v)/(1-w) + gamma*(1-p_m)v(1-w-v) + gamma*q_m*y*(1-w-v)]
B = solve(eqns2, syms)

%filename = "equilibria_eigenvalues.xlsx"
%the code below failed to 'start the Excel server' so it made a bunch of
%.csv files...hence why I gave them different names bc only 1 sheet
xlswrite("twopatch_fullsolve.xlsx",data,'All data','A1')


