clear  % Clears command window
clc    % Clears command history
clf    % Removes anything in the figure window before simulation.

r = 0.55
a = 0.1
d = 0.24
gamma = 0.77
epsilon = [0.005 0.01 0.05 0.1 0.5]
zeta = [0.005 0.01 0.05 0.1 0.5]
g_vec = 0.01:0.01:0.99
%(rows, columns)
data = 2.*ones(9900,12) %see 11.9.2018 notes
%column 1 = g, column 2,3,4,5 = recruitment values, column 6 = equi (1 - 20
%repeated), column 7,8,9,10 = equi values, column 11,12,13,14 =
%eigenvalues
gvecvec = repmat(0.01:0.01:0.99, [100 1])
gvecvec = gvecvec(:) %turns it into a vector, length(gvecvec) = 9900
recr_vec = repmat([0.005 0.01 0.05 0.1 0.5], [20 99]) %20 each element, repeat whole thing 99 times
recr_vec = recr_vec(:)
equinum = repmat(1:20, [1 495])
equinum = equinum(:)
data(1:end,1) = gvecvec %column 1 = g
data(1:end,2) = recr_vec %epsilon
data(1:end,3) = recr_vec %zeta
data(1:end,4) = equinum

for i = 0:495
syms x y w v
eqns = [r*(1-x-y)*x - d*x - a*x*y + data((i*20)+1,3)*w*(1-x-y) == 0, a*x*y - (data((i*20)+1,1)*y)/(1-x) + gamma*y*(1-x-y) + data((i*20)+1,2)*v*(1-x-y) == 0, r*(1-w-v)*w - d*w - a*w*v + data((i*20)+1,3)*x*(1-w-v) == 0, a*w*v - (data((i*20)+1,1)*v)/(1-w) + gamma*v*(1-w-v) + data((i*20)+1,2)*y*(1-w-v) == 0];
init_guess = [0 1; 0 1; 0 1; 0 1];
S = vpasolve(eqns, [x,y,w,v], init_guess) %some sol'ns with numerical instabilities but those are all ones outside the specified range

%let c1 = x, m1 = y, c2 = w, m2 = v
%Xequi(1:length(S.x),i) = S.x;
len = 20 - length(S.x);
data((i*20)+1:((i+1)*20)-len,5) = S.x;
data((i*20)+1:((i+1)*20)-len,6) = S.y;
data((i*20)+1:((i+1)*20)-len,7) = S.w;
data((i*20)+1:((i+1)*20)-len,8) = S.v;


eqns2 = [r*(1-x-y)*x - d*x - a*x*y + data((i*20)+1,3)*w*(1-x-y), a*x*y - (data((i*20)+1,1)*y)/(1-x) + gamma*y*(1-x-y) + data((i*20)+1,2)*v*(1-x-y), r*(1-w-v)*w - d*w - a*w*v + data((i*20)+1,3)*x*(1-w-v), a*w*v - (data((i*20)+1,1)*v)/(1-w) + gamma*v*(1-w-v) + data((i*20)+1,2)*y*(1-w-v)];

J = jacobian(eqns2, [x,y,w,v])
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

%filename = "equilibria_eigenvalues.xlsx"
%the code below failed to 'start the Excel server' so it made a bunch of
%.csv files...hence why I gave them different names bc only 1 sheet
xlswrite("twopatch_fullsolve.xlsx",data,'All data','A1')


