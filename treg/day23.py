#!/usr/bin/python
import intcode
import fileinput
import collections
import math

inputQueues = {i:[] for i in list(range(50)) + [255]}

def outGen(id):
    packet = []
    def outputter(x):
        nonlocal packet
        global inputQueues

        packet.append(x)
        
        if len(packet) == 3:
            inputQueues[packet[0]].append(packet[1:])
            packet = []

    return outputter

def inGen(id):        
    def input():
        global inputQueues
        
        # Provide network ID first
        yield id
            
        while True:
            if len(inputQueues[id]) == 0:
                yield -1
            else:
                pkt = inputQueues[id].pop(0)
                yield pkt[0]
                yield pkt[1]
                
    return input
                    
def runNetwork(ins):
    global inputQueues
    nics = [intcode.CPU(ins, inGen(x), outGen(x)) for x in range(50)]
    while not all([nic.finished() for nic in nics]):
        for n in nics:
            n.run(1)

        if len(inputQueues[255]) != 0:
            raise Exception("Part 1: ", inputQueues[255][0][1])

def runNATnetwork(ins):
    global inputQueues
    
    nics = [intcode.CPU(ins, inGen(x), outGen(x)) for x in range(50)]
    previousY = [math.inf]            
    
    while not all([nic.finished() for nic in nics]):
        for n in nics:
            n.run(1)

        # Check the NAT status - remember only last packet
        if len(inputQueues[255]) > 1:
            inputQueues[255] = [inputQueues[255][-1]]

        # Check network status
        if all([len(inputQueues[x]) == 0 for x in range(50)] + [len(inputQueues[255]) > 0]):
        
            # It's idle! Send to 0, but check for the double send
            previousY.append(inputQueues[255][0][1])
            if previousY[0] == previousY[1]:
                raise Exception("Part 2: Sent Y", previousY[0], "back to back from NAT")
            else:
                previousY.pop(0)
                
            inputQueues[0].append(inputQueues[255].pop())
            
def main():
    ins = [int(x) for x in next(fileinput.input()).strip().split(',')]    
    try:
        runNetwork(ins)
    except Exception as e:
        print(e)

    try:
        runNATnetwork(ins)
    except Exception as e:
        print(e)
        
if __name__ == "__main__":
    main()
