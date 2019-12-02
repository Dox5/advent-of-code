import intcode
import pytest

@pytest.mark.parametrize("ins, ans, offset", [([1,2,3,3],[1,2,3,6],4),([1,0,1,0],[1,0,1,0],4)])
def test_add(ins, ans, offset):
    assert intcode.add(ins, 0) == offset
    assert ins == ans

@pytest.mark.parametrize("ins, ans, offset", [([2,0,3,1],[2,2,3,1],4)])
def test_mul(ins, ans, offset):
    assert intcode.mul(ins, 0) == offset
    assert ins == ans

@pytest.mark.parametrize("ins, ans, res", [([1,3,5,99],[1,3,5,99],1)])
def test_end(ins, ans, res):
    with pytest.raises(intcode.EndOfComputation) as excinfo:
        intcode.end(ins, 3)

    assert ins == ans
    assert excinfo.value.result == res

@pytest.mark.parametrize("ins, ans", [([1,9,10,3,2,3,11,0,99,30,40,50],3500),([1,0,0,0,99],2),([2,3,0,3,99],2),([2,4,4,5,99,0],2),([1,1,1,4,99,5,6,0,99],30)])
def test_cpu(ins, ans):
    assert intcode.runComputer(ins) == ans
