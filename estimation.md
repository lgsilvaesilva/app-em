<font size=3px>
Now we'll fit the Gaussian mixture model for each variable, ``waiting`` and ``eruptions``. Assume that each variable set \\(\mathbf{x} = (\mathbf{x}_1,\mathbf{x}_2,\ldots,\mathbf{x}_n)\\)
 is a sample of \\(n\\) independent observations from a mixture two univariate normal distributions, and let \\(\mathbf{z} = (\mathbf{z}_1,\mathbf{z}_2,\ldots,\mathbf{z}_n)\\) be the latent variable that determine the component from which the observation originates.
$$
{X}_i|\left[\theta;{Z}_i=1 \right] \sim \mathcal{N}\left(\mu_1, \sigma_1\right) \text{ and } {X}_i|\left[\theta; {Z}_i=2\right] \sim \mathcal{N}\left(\mu_2, \sigma_2\right)
$$
where \\(\mathbb{P}({Z}_i=1)={\omega}_1\\) and \\(\mathbb{P}({Z}_i=2)={\omega}_2=1-{\omega}_1\\). The aim is to estimate the unknown parameters representing the "mixing" value between the Gaussian the means and variances of each:
$$\theta=\left(\mu_1,\mu_2,\sigma_1,\sigma_2,\omega\right)$$.

The ``mixtools`` package is one of several available in **[R](http://www.r-project.org/ "Title")** to fit mixture distributions. Using the function ``normalmixEM()`` for fitting normal mixture densities.


```r
> library(mixtools)
> data(faithful)
> wait.mix <- normalmixEM(faithful$waiting)
> erup.mix <- normalmixEM(faithful$eruptions)
```



```
               Waiting-1 Waiting-2 Eruption-1 Eruption-2
Mean             54.6149   80.0911     2.0186     4.2733
Std. Deviation    5.8712    5.8677     0.2356     0.4371
Weight            0.3609    0.6391     0.3484     0.6516
```

### Classification
The problem of classification is to find how many groups exists and what group each observation belongs to. In this example is clear the there two groups. Therefore we used EM algorithm for fitting a mixture of two multivariate normal distributions, as showed in the animation below:



</font>
