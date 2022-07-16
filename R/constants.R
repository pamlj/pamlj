j_DEBUG=T
j_INFO=T
t_INFO=T



TRANS_WARNS<-list()

INTRO<-"<b> Getting started</b>:<br> To produce a dataset useful for simulations, please start with an empty dataset.
        Define the name of the <b>dependent variable</b> and the number of participants per cluster.
        Then specify the number of clusters for each <b>clustering variable</b>.
        If no clustering variable is required, the `N per cluster` will be the total sample N. 
        If clustering variables are defined, the total sample N will be `N per cluster` times `N clusters`.
        <br>For <b>factors</b>, please specify the number of levels (categories) to simulate.<br>
        If the dataset is not empty, the name of the variables to simulate should not be present in the dataset. <br>
        When the setup is ok, please select <b>Produce the data</b> option to transfer the
        data in the dataset."