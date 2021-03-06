"""
Created on Thu May  2 21:47:08 2019

@author: brunamirelle

Modified by: Rafael Felipe Bressan (https://github.com/rfbressan)
Date: 2022-02-10
"""

import numpy as np
from pandas import value_counts
from scipy.optimize import fsolve

# Resolve o modelo de Artuc (2008)


class EconomiaArtuc:

    def __init__(self, alpha, beta, C, nu, K_X, K_Y, L_bar, p0):
        """Inicializa economia

        :param alpha: parâmetro da função de produção (Cobb-Douglas)
        :param beta: fator de desconto dos trabalhadores
        :param C: custo de mudar de setor
        :param K_X, K_Y: dotações de capital em cada setor
        :param L_bar: dotação de trabalho na economia
        :param p0: preço pré-liberalização do bem X
        :return: objeto da classe EconomiaArtuc
        """
        # Checks alpha
        if self.valid_alpha_bool(alpha):
            self.alpha = alpha
        else:
            raise ValueError(
                f'Alpha parameter must be between zero and one, open set. Value passed was: {alpha}')

        # Checks beta
        if self.valid_beta_bool(beta):
            self.beta = beta
        else:
            raise ValueError(
                f'Beta parameter must be between zero and one, open set. Value passed was: {beta}')

        self.C = C
        self.nu = nu
        self.K_X = K_X
        self.K_Y = K_Y
        self.L_bar = L_bar
        self.p0 = p0

    def valid_alpha_bool(self, alpha):
        """Checks whether alpha is in (0, 1)

        :param alpha: Cobb-Douglas parameter
        :type alpha: float
        :return: a flag indicating alpha is in (0, 1)
        :rtype: bool
        """
        return (alpha > 0) and (alpha < 1)

    def valid_beta_bool(self, beta):
        """Checks whether beta is in (0, 1)

        :param beta: Preferences discount factor
        :type beta: float
        :return: a flag indicating beta is in (0, 1)
        :rtype: bool
        """
        return (beta > 0) and (beta < 1)

    # Função Omega no paper
    def Omega(self, mu):
        """Valor da opção do trabalhador

        :param mu: moving cost
        :type mu: float
        :return: value
        :rtype: float
        """
        return self.nu*np.log(1 + np.exp(mu/self.nu))

    # CDF da distribuição logística (diferença dos choques)
    def G(self, mu):
        if mu == np.Inf:
            return 1.0
        else:
            return np.exp(mu/self.nu)/(1 + np.exp(mu/self.nu))

    # Condição de primeira ordem para maximização dos lucros
    def FOC(self, L, K, p):
        return p*self.alpha*((L)**(self.alpha-1))*((K)**(1-self.alpha))

    # Indice de preços
    def phi(self, p):
        return p**(.5)

    # Retorna as equações necessárias para calcular steady-state
    # vec_guess - valores, salários e thresholds (vetor)
    # p - preço relativo do bem X no steady-state desejado
    def steady_discrep(self, vec_guess, p):
        """Equações necessárias para steady-state

        :param vec_guess: initial guess vector
        :type vec_guess: ndarray of floats
        :param p: price
        :type p: float
        :return: vector of values
        :rtype: ndarray of floats
        """
        vx, vy, mux, muy, lx, ly, wx, wy = vec_guess
        discrep = np.zeros(8)
        discrep[0] = wx - self.FOC(lx, self.K_X, p)/self.phi(p)
        discrep[1] = wy - self.FOC(ly, self.K_Y, 1)/self.phi(p)
        discrep[2] = lx + ly - self.L_bar
        discrep[3] = mux - self.beta*(vy-vx) + self.C
        discrep[4] = muy - self.beta*(vx-vy) + self.C
        discrep[5] = vx - wx - self.beta*(vx) - self.Omega(mux)
        discrep[6] = vy - wy - self.beta*(vy) - self.Omega(muy)
        discrep[7] = lx - (1-self.G(mux))*lx - self.G(muy)*ly
        #discrep[8] = L_Y - (1-self.G(mu_Y))*L_Y - self.G(mu_X)*L_X
        return discrep

    # Calcula estado estacionário para um dado valor do preço externo
    def solve_steady(self, p):
        def steady_price(vec_guess):
            return self.steady_discrep(vec_guess, p)
        return fsolve(steady_price, np.ones(8))

    def solve_model(self, p_after, T_SS=30, effective_at=0, tol=1e-5):
        """Resolve o modelo para um dado valor pós-liberalização

        :param p_after: preço relativo de X pós-liberalização
        :param T_SS: número de períodos até novo steady-state (padrão 30, t=31 é steady-state)
        :param effective_at: a partir de que período vale a abertura (padrão é o zero)?
        :param tol: Tolerância para convergência do algoritmo
        :return: time path of endogenous variables
        :rtype: dictionary
        """
        # Calcula valores do steady-state inicial e final
        # Inicial
        vx0, vy0, mux0, muy0, lx0, ly0, wx0, wy0 = self.solve_steady(self.p0)
        # Final
        vxt, vyt, muxt, muyt, lxt, lyt, wxt, wyt = self.solve_steady(p_after)

        # Chutes iniciais para a trajetória dos valores em cada setor
        zxt = np.linspace(vx0, vxt, T_SS + 1)
        zyt = np.linspace(vy0, vyt, T_SS + 1)

        # Trajetória de preços
        p_t = np.concatenate(
            (np.repeat(self.p0, effective_at),
             np.repeat(p_after, T_SS + 1 - effective_at))
        )

        # print(p_t)

        err = tol + 1

        while err > tol:
            # Calcula trajetórias para mu_X e mu_Y
            mu_X_t = self.beta*(np.concatenate((zyt[1:], [vyt])) -
                                np.concatenate((zxt[1:], [vxt]))) - self.C
            mu_Y_t = self.beta*(np.concatenate((zxt[1:], [vxt])) -
                                np.concatenate((zyt[1:], [vyt]))) - self.C

            L_X_t = np.empty(T_SS + 1)
            L_Y_t = np.empty(T_SS + 1)

            L_X_t[0] = lx0
            L_Y_t[0] = ly0

            for tt in range(1, T_SS + 1):
                L_X_t[tt] = (1-self.G(mu_X_t[tt-1]))*L_X_t[tt-1] +\
                    self.G(mu_Y_t[tt-1])*L_Y_t[tt-1]
                L_Y_t[tt] = self.L_bar - L_X_t[tt]

            w_Y_t = self.FOC(L_Y_t, self.K_Y, 1)/self.phi(p_t)
            w_X_t = self.FOC(L_X_t, self.K_X, p_t)/self.phi(p_t)

            z_tilde_X_t = w_X_t + self.beta * \
                np.concatenate((zxt[1:], [vxt])) + self.Omega(mu_X_t)
            z_tilde_Y_t = w_Y_t + self.beta * \
                np.concatenate((zyt[1:], [vyt])) + self.Omega(mu_Y_t)

            err = np.max((np.abs(z_tilde_X_t - zxt),
                         np.abs(z_tilde_Y_t - zyt)))

            zxt = z_tilde_X_t
            zyt = z_tilde_Y_t

            print(err)

        return {
            'wx': np.concatenate(([wx0], w_X_t, [wxt])),
            'wy': np.concatenate(([wy0], w_Y_t, [wyt])),
            'Vx': np.concatenate(([vx0], zxt, [vxt])),
            'Vy': np.concatenate(([vy0], zyt, [vyt])),
            'Lx': np.concatenate(([lx0], L_X_t, [lxt])),
            'Ly': np.concatenate(([ly0], L_Y_t, [lyt]))
        }


# Script for testing purposes
if __name__ == "__main__":
    # execute only if run as a script
    # This is a streamlit application, run it as such:
    # $ streamlit run EconomiaArtuc.py
    import matplotlib.pyplot as plt
    import streamlit as st

    st.set_page_config(
        page_title="Artuç Simulation",
        page_icon=":dollar:",
        layout="wide",
        initial_sidebar_state="expanded",
    )

    st.title("Artuç et. al. (2008) replication")
    # Solving the model and ploting wages
    # Uses sidebar for economy parameters
    # alpha, beta, C, nu, K_X, K_Y, L_bar, p0
    alpha = st.sidebar.number_input(
        "alpha", min_value=0.01, max_value=0.99, value=0.5)
    beta = st.sidebar.number_input(
        'beta', min_value=0.01, max_value=0.99, value=0.97)
    # C = st.sidebar.number_input('C', value=1)
    nu = st.sidebar.number_input('nu', value=0.31)
    # kx = st.sidebar.number_input('K_X', value=1)
    # ky = st.sidebar.number_input('K_Y', value=1)
    # lbar = st.sidebar.number_input('L_bar', value=2)
    # p0 = st.sidebar.number_input('p0', value=1)

    econ_args = {'alpha': alpha,
                 'beta': beta,
                 'C': 1,
                 'nu': nu,
                 'K_X': 1,
                 'K_Y': 1,
                 'L_bar': 2,
                 'p0': 1}
    econ = EconomiaArtuc(**econ_args)
    eff_at = st.sidebar.number_input("Period where the tariff is actually reduced:",
                                     value=10,
                                     min_value=1,
                                     max_value=12,
                                     step=1,
                                     format="%d")
    pw = st.sidebar.number_input('pW', value=0.7)
    sol = econ.solve_model(pw, effective_at=int(eff_at))

    fig1, ax1 = plt.subplots()
    ax1.plot(sol['wx'], label='Sector X')
    ax1.plot(sol['wy'], label='Sector Y')
    w_extended = np.append(sol['wx'], sol['wy'])
    ax1.vlines(x=eff_at,
               ymin=min(w_extended),
               ymax=max(w_extended),
               colors='red', linestyles='dashed')
    ax1.set_title("Delayed to t = " + str(eff_at))
    ax1.set_xlabel("time")
    ax1.set_ylabel("Wages")
    ax1.legend()

    fig2, ax2 = plt.subplots()
    ax2.plot(sol['Lx'], label='Sector X')
    ax2.plot(sol['Ly'], label='Sector Y')
    l_extended = np.append(sol['Lx'], sol['Ly'])
    ax2.vlines(x=eff_at,
               ymin=min(l_extended),
               ymax=max(l_extended),
               colors='red', linestyles='dashed')
    ax2.set_title("Delayed to t = " + str(eff_at))
    ax2.set_xlabel("time")
    ax2.set_ylabel("Labor force")
    ax2.legend()

    col1, col2 = st.columns(2)
    with col1:
        st.pyplot(fig1)
    with col2:
        st.pyplot(fig2)
