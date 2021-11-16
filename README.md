# Emotional-Response-Analysis
Understanding correlation between Emotions and EEG brain-waves using R-Programming

	• Specifically picked up 4 types of pre-dominant emotion videos : Joy, Anger, Surprise, Sad.
	• These emotions are analogous to primary colours which are used to define all the other colours.
	• Operate and analyse these emotions using Think-Gear Connector on our viewer dataset (sample size: 10) and then document our findings and conclusion.
	• We receive alpha, beta, delta and theta frequency values in Hz from think-gear connector as an csv file.  
	• Approach: 
		○ first take in the mean frequency values from our viewer dataset as input 
		○ operate on these values by filtering outliers.
		○ normalizing the data and then the function would return us the extent of specific emotions involved with that particular advertisement for any specific timeframe.
		○ Multiple graphs plotted using R against different sample members to find pattern.
	• Final Conclusions achieved :
		○ Low and Constant theta values suggest low level of stress suggesting viewer is at ease during the entire duration of the clip.
		○ Sad emotion is the easiest identifiable emotion from the output values because of continuous fluctuating delta waves repeatedly seen during sad instances of video-clips           which highlight sense of empathy experienced by viewers
    ○ Extremely high Delta value peaks definitely indicate shock and surprise
