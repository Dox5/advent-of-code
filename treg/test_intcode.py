import intcode
import pytest

@pytest.mark.parametrize("ins, ans", [([1,9,10,3,2,3,11,0,99,30,40,50],3500),([1,0,0,0,99],2),([2,3,0,3,99],2),([2,4,4,5,99,0],2),([1,1,1,4,99,5,6,0,99],30)])
def test_cpu(ins, ans):
    c = intcode.CPU(ins)
    c.run()
    assert c.memory[0] == ans

@pytest.mark.parametrize("ins, ans", [([1002,4,3,4,33],[1002,4,3,4,99])])
def test_modes(ins, ans):
    c = intcode.CPU(ins)
    c.run()
    assert ans == c.memory

def test_branching():
    ins = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]

    out = []
    outputer = lambda x : out.append(x)
    inputer = lambda : (x for x in [5,8,10])
    
    c = intcode.CPU(ins,inputer,outputer)
    for i in range(3):
        c.run()
        c.reset()

    assert out == [999,1000,1001]
    
