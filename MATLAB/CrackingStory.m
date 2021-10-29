% Script for figure generation for figures used in Cracking the Uncrackable
% Code doc

alph = 'abcdefghijklmnopqrstuvwxyz';

%% Initial letter frequency histograms
alphCell = {};
for idx = 1:length(alph); 
    alphCell{idx} = alph(idx); 
    counts(idx) = sum(f18.nsnpl==alph(idx)); 
end
[countSort,sortOrder] = sort(counts);
[countSort,sortOrder] = sort(counts,'descend');
alphCellSort = alphCell(sortOrder);
figure(50); 
histogram('Categories',alphCellSort, 'BinCounts', countSort/sum(countSort)*100)
xlabel('Ciphertext Letter');
ylabel('Ciphertext Frequency (%)');
ylim([0,14])


engFreqPcts=[12.60 9.37 8.34 7.70 6.80 6.71 6.11 6.11 5.68 4.24 4.14 2.85 ...
    2.73 2.53 2.34 2.04 2.03 1.92 1.66 1.54 1.06 0.87 0.23 0.20 0.09 0.06];
engLettFreqOrder = 'etaonihsrlducmwyfgpbvkjxqz';
engCell = {};
for idx = 1:length(engLettFreqOrder)
    engCell{idx} = engLettFreqOrder(idx);
end
figure(51);
histogram('Categories',engCell, 'BinCounts', engFreqPcts)
xlabel('English Letter');
ylabel('Typical Frequency (%)');





