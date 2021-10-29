function interlaced = interlace(M1, M2)
% interlaces the rows of two matrices of identical size

for i = 1:size(M1,1)
    M3(2*i-1,:) = M1(i,:);
    M3(2*i,:) = M2(i,:);
end

interlaced = M3;
