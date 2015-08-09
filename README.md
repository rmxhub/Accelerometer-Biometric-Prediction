Data Preparation

    After removing from the data set samples from devices with fewer than 6000 samples collected and sequences of samples during periods of rest exceeding 10 seconds, 60 million samples from 387 users were made available for the competition.
    Samples from each device were split chronologically into train and test sets, with the earliest samples assigned to the train set.
    The test set was demarcated into 90,000 consecutive sequences of 300 samples each from a same device and each sequence assigned a unique Sequence Id.
    Each sequence Id was associated with a professed device Id such that in some cases a true device id was assigned and in others a false device id was assigned.



Field name  Description

T   Unix time (miliseconds since 1/1/1970)

X   Acceleration measured in g on x co-ordinate

Y   Acceleration measured in g on y co-ordinate

Z   Acceleration measured in g on z co-ordinate

DeviceId  Unique Id of the device that generated the samples

SequenceId  Unique sequence number assigned to each question. Each group of samples is labeled with a unique SequenceId. Each SequenceId is matched to a professed DeviceId in the questions.csv file.