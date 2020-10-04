import statistics
import random
from collections import Counter

ages = [31, 26, 34, 37, 27, 26, 32, 32, 26, 27, 27, 24, 32, 33, 27, 25, 26, 38, 37, 31, 34, 24, 33, 29, 26]
class Statistics:
    def __init__(self,ages):
        self.ages = ages
    def count(self):
       return len(self.ages)
    def mysum(self):
        return sum(self.ages)
    def mymin(self):
        return min(self.ages)
    def mymax(self):
        return max(self.ages)
    def myrange(self):
        return random.choice(self.ages)
    def mean(self):
        return statistics.mean(self.ages)
    def median(self):
        return statistics.median(self.ages)
    def mode(self):
        modecount = Counter(self.ages)
        return (modecount.most_common()[:1])[0]
    def var(self):
        return statistics.variance(self.ages)
    def std(self):
        return statistics.stdev(self.ages)
    def freq_dist(self):
        ele_dict = {}
        for ele in self.ages:
            if ele in ele_dict:
                ele_dict[ele] += 1
            else :
                ele_dict[ele] = 1
        for ele,freq in ele_dict.items():
            ele_dict[ele] = (freq/len(self.ages))*100
        ele_list = list(ele_dict.items())
        ele_list.sort(key = lambda x:x[1],reverse = True)
        return ele_list
            
data = Statistics(ages)
print('Count:', data.count()) 
print('Sum: ', data.mysum()) 
print('Min: ', data.mymin()) 
print('Max: ', data.mymax()) 
print('Range: ', data.myrange()) 
print('Mean: ', data.mean()) 
print('Median: ',data.median()) 
print('Mode: ', data.mode()) 
print('Variance: ',data.var()) 
print('Standard Deviation: ', data.std()) 
print('Frequency Distribution: ',data.freq_dist()) 
