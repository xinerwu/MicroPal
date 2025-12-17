% Extract sea ice concentration as average of 1955-2012 (to stay in line
% with the n=1968 database sea ice calibration data, see de Vernal et al.,
% 2020). Computes annual mean (%) and sea ice duration as the number of 
% months with sea ice > 50% (also to be in line with n=1968).

% set query point
londeg=-46; lonmin=-9.290;
latdeg=60; latmin=56.206;
tarlon=londeg+lonmin/60;
tarlat=latdeg+latmin/60;

% choose inter-/extra-polation method ('linear','nearest','natural')
method='nearest';

% load pre-treated sea ice concentration data
load("matlab.mat","lon","lat","seaiceconc_1955_2012","seaiceconc_1955_2012_mean")
lon(lon>180)=lon(lon>180)-360;

% filter out land points (=120)
idx=find(seaiceconc_1955_2012_mean~=120);

% interpolate to query point
F=scatteredInterpolant(lon(idx),lat(idx),seaiceconc_1955_2012_mean(idx),method,method);
annual=F(tarlon,tarlat);

% hot swap the values and loop through months
seaiceconc_1955_2012=double(seaiceconc_1955_2012);
monthly=zeros(1,size(seaiceconc_1955_2012,3));
for mo=1:size(seaiceconc_1955_2012,3)
    tmp=seaiceconc_1955_2012(:,:,mo);
    F.Values=tmp(idx);
    monthly(mo)=F(tarlon,tarlat);
end

% reshape the monthly data and find out number of months with SIC>50%
yearly=reshape(monthly,12,[]);
ice50=yearly>50;
nb=sum(ice50,1);
avg_nb=mean(nb);
std_nb=std(nb);

% print results
fprintf('Average annual mean sea ice concentration is %.2f%%\n',annual)
fprintf('Average sea ice duration is %.1f+/-%.1f months/year\n',avg_nb,std_nb)