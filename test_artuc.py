from multiprocessing.sharedctypes import Value
import pytest
import numpy as np

from EconomiaArtuc import EconomiaArtuc

@pytest.mark.parametrize("alpha", [0, 1])
def test_EconomiaArtuc_alpha(alpha):
    args = {
        'beta': 0.97,
        'C': 1,
        'nu': 0.31,
        'K_X': 1,
        'K_Y': 1,
        'L_bar': 2,
        'p0': 1
    }
    with pytest.raises(ValueError):
        EconomiaArtuc(alpha, **args)

@pytest.mark.parametrize("beta", [0, 1])
def test_EconomiaArtuc_beta(beta):
    args = {
        'alpha': 0.5,
        'C': 1,
        'nu': 0.31,
        'K_X': 1,
        'K_Y': 1,
        'L_bar': 2,
        'p0': 1
    }
    with pytest.raises(ValueError):
        EconomiaArtuc(beta = beta, **args)

@pytest.fixture
def econ():
    args = {'alpha': 0.5,
            'beta': 0.97,
            'C': 1,
            'nu': 0.31,
            'K_X': 1,
            'K_Y': 1,
            'L_bar': 2,
            'p0': 1}
    return EconomiaArtuc(**args)


@pytest.mark.parametrize("mu, expected",
                         [(0, 0.31*np.log(2)),
                          (-np.Inf, 0.0)])
def test_Omega(econ, mu, expected):
    assert econ.Omega(mu) == expected


@pytest.mark.parametrize("mu, expected",
                         [(-np.Inf, 0.0),
                          (np.Inf, 1)])
def test_G(econ, mu, expected):
    assert econ.G(mu) == expected
