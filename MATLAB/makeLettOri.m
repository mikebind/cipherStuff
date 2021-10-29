function [lett_ori, oddities] = makeLettOri(tori,frame,lett)

lett_ori = repmat('-', size(tori));
lett_ori(tori==lett) = frame.ori(tori==lett);
lett_ori(tori==' ') = '|'; % for boundaries

% Check for oddities
oddities = repmat('-', size(tori));
oddities(tori==' ') = '|'; % mark boundaries
for alphNum = 1:size(tori,2)
    ciphLettVect = frame.ori(tori(:,alphNum)==lett, alphNum);
    uniqCiphLetts = unique(ciphLettVect);
    counts = zeros(size(uniqCiphLetts));
    for ciphLettIdx = 1:length(uniqCiphLetts)
        counts(ciphLettIdx) = sum(ciphLettVect==uniqCiphLetts(ciphLettIdx));
    end
    % Find the two most common, any others are oddities
    remainingUniq = uniqCiphLetts;
    remainingCounts = counts;
    while length(remainingUniq)>2
        % remove the letter with the min counts as an oddity
        [~,idx] = min(remainingCounts);
        odd_lett = remainingUniq(idx);
        oddities(lett_ori(:, alphNum)==odd_lett, alphNum) = '#';
        remainingUniq(idx) = [];
        remainingCounts(idx) = [];
    end
    for idx = 1:2
        good_lett = remainingUniq(idx);
        oddities(lett_ori(:, alphNum)==good_lett, alphNum) = 'o';
    end
end
    