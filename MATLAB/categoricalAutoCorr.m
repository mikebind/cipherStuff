function [r,lags] = categoricalAutoCorr(v, maxLag)
if nargin<2 || isempty(maxLag)
    %maxLag = length(v)-1;
    maxLag = round(0.75 * length(v));
end

lags = 0:maxLag;

for lag = 0:maxLag
    v1 = v(lag+1:end);
    v2 = v(1:end-lag);
    r(lag+1) = sum(v1==v2)/length(v1);
    % Dividing by the length normalizes so that a value of 1 is always
    % possible, and is always the max possible.
end



