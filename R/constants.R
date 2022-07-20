j_DEBUG=T
j_INFO=T
t_INFO=T



TRANS_WARNS<-list()

INTRO<-"<b> Getting started</b>:<br> To produce a dataset useful for simulations, please start with an empty dataset.
        Define the name of the <b>dependent variable</b> and the number of participants per cluster.
        Then specify the number of clusters for each <b>clustering variable</b>.
        The total sample N will be `N per cluster` times `N clusters`. 
        In case of multiple clustering variables, `N per cluster` is the N within each combinations of clusters
        <br>For <b>factors</b>, please specify the number of levels (categories) to simulate.<br>
        If the dataset is not empty, the name of the variables to simulate should not be present in the dataset. <br>
        When the setup is ok, please select <b>Produce the data</b> option to transfer the
        data in the dataset. <br>
        All variables are uncorrelated, their relationships can be defined in the power analysis module."

BRIEF<-"<b>Available actions</b>:<br> One can use the produced data to estimate power parameters
         of the mixed model. Although one can work directly on the active dataset, it is reccomanded
         to save the data as a <b>csv file</b>, so the data will not be altered in future sessions."