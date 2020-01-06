from day1 import calcFuel
import pytest

@pytest.mark.parametrize("moduleweight, fuel", [(12,2),(14,2),(1969,654),(100756,33583)])
def test_fuelcalc(moduleweight, fuel):
    assert calcFuel(moduleweight) == fuel, "failed"

@pytest.mark.parametrize("moduleweight, fuel", [(14,2),(1969,966),(100756,50346)])
def test_complexfuelcalc(moduleweight, fuel):
    assert calcFuel(moduleweight, 1) == fuel, "failed"
