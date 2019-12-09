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
    for i in range(len(ans)):
        assert ans[i] == c.memory[i]

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

def test_full_copy():
    ins = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
    out = []
    outputer = lambda x : out.append(x)
    c = intcode.CPU(ins,None,outputer)
    c.run()
    assert ins == out

def test_full_bugnum():
    ins = [1102,34915192,34915192,7,4,7,99,0]
    out = []
    outputer = lambda x : out.append(x)
    c = intcode.CPU(ins,None,outputer)
    c.run()    
    assert len(str(out[-1])) == 16

def test_full_bignum2():
    ins = [104,1125899906842624,99]
    out = []
    outputer = lambda x : out.append(x)
    c = intcode.CPU(ins,None,outputer)
    c.run()
    assert out[-1] == ins[1]
    
