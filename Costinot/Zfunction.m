%%% Matlab function needed to perform counterfactuals for "What Goods Do Countries Trade? A Quantitative Exploration of the Ricardian Model" 
%%% By Arnaud Costinot, Dave Donaldson and Ivana Komunjer
%%% Review of Economic Studies, 2011


function [F] = Zfunction(Z_prime)
%%% NB: Z_prime is defined as (Z)^(theta).

global  ALPHA Gamma N K theta ref z PI
clear ZZ2 denom Ksumterm Ksum Jsumterm Jsum

ZZ2 = ones(N,N,K);
for i = 1:N
    for j = 1:N
        for k = 1:K
            ZZ2(i,j,k) = Z_prime(i)*(z(i,k).^(-theta))*PI(i,j,k);
        end
    end
end 
  
denom = ones(N,K);
for j = 1:N
    for k = 1:K
        denom(j,k) = sum(ZZ2(:,j,k),1);
    end
end

Ksumterm = ones(N,N,K);
for i = 1:N
    for j = 1:N
        for k = 1:K
        Ksumterm(i,j,k) = ALPHA(j,k)*ZZ2(i,j,k)/denom(j,k);
        end
    end
end
  
Ksum(:,:) = sum(Ksumterm,3);

Jsumterm = ones(N,N);
for j = 1:N
    Jsumterm(:,j) = Gamma(j)*Ksum(:,j);
end
   
Jsum = sum(Jsumterm,2);

F = ones(N,1);
for i = 1:N
    if i==ref
    	F(ref) = Z_prime(ref) - 1;
    else
        F(i) = Jsum(i) - Gamma(i) ;
    end    
end
  

clear ZZ2 denom Ksumterm Ksum Jsumterm Jsum

