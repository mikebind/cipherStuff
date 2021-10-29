function findspac(string,nsnp)

k = findstr(lower(nsnp),string);
lines = findline(string,nsnp);

for i = 1:length(k)-1
    spac(i) = k(i+1)-k(i);
    shifts(i) = rem(spac(i),18);
end

%lines
%shifts
s1 =sprintf('Line Number    Spacing    Shift     ');
disp(s1);
for i = 1:length(shifts)
    s2 = sprintf('  %2.0d             %2.0d       %+2.0d    ', lines(i),spac(i),shifts(i));
    disp(s2)
end
s3 = sprintf('  %2.0d   \n',lines(i+1));
disp(s3)