function N = number_of_crossings(start,fin,spacing,offset)
if nargin<4
    offset=0;
end

N = sum(mod((start+1:fin-1) + offset, spacing) == 0);
% + and - 1 because don't want to count transitions which 
% occur AT the actual boundaries