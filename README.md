# Time Series Final Project
###### 以下敘述欲助於快速檢查邏輯是否有誤
簡述前處理及差分過程:
* 先觀察 time series plot，放大後發現每 7 個 lag 有一個規律，acf 亦有規律地出現高峰，故決定先做一次季節性差分。(s=7, D=1)
* 再次觀察 acf、pacf，看起來還需要做一次差分。(d=1)


將兩種資料都進行模型配適（所有資料、前六百筆資料），並列出所有可考慮之 order:
```diff
- 以下配適過程僅考慮ACF及PACF，尚未解決殘差問題
```
1. 所有資料
    * non-seasonal:
        * acf cuts off after lag 1/3/4, pacf tails off; (p, q)=[(0, 1), (0, 3), (0, 4)]
        * acf tails off, pacf cuts off after lag 2; (p, q)=(2, 0)
        * acf tails off, pacf tails off; (p, q)=[(2, 1), (2, 3), (2, 4)]
    * seasonal:
        * acf cuts off after lag 1, pacf tails off; (P, Q)=(0, 1)
        * acf tails off, pacf tails off; (P, Q)=(2, 1)
    <br>
    <table>
      <tr>
        <th>Model</th>
        <th>AIC</th>
        <th>lowest</th>
      </tr>
      <tr>
        <td> <img src="https://render.githubusercontent.com/render/math?math=ARIMA(0, 1, 1)\times(0, 1, 1)_7"> </td>
        <td>26.07277</td>
        <td></td>
      </tr>
      <tr>
        <td> <img src="https://render.githubusercontent.com/render/math?math=ARIMA(0, 1, 3)\times(0, 1, 1)_7"> </td>
        <td>26.06357</td>
        <td></td>
      </tr>
      <tr>
        <td> <img src="https://render.githubusercontent.com/render/math?math=ARIMA(0, 1, 4)\times(0, 1, 1)_7"> </td>
        <td>26.06554</td>
        <td></td>
      </tr>
      <tr>
        <td> <img src="https://render.githubusercontent.com/render/math?math=ARIMA(2, 1, 0)\times(0, 1, 1)_7"> </td>
        <td>26.06524</td>
        <td></td>
      </tr>
      <tr>
        <td> <img src="https://render.githubusercontent.com/render/math?math=ARIMA(2, 1, 4)\times(0, 1, 1)_7"> </td>
        <td>26.01717</td>
        <td></td>
      </tr>
      <tr>
        <td> <img src="https://render.githubusercontent.com/render/math?math=ARIMA(2, 1, 4)\times(2, 1, 1)_7"> </td>
        <td>25.9925</td>
        <td></td>
      </tr>
      <tr>
        <td> <img src="https://render.githubusercontent.com/render/math?math=ARIMA(2, 1, 3)\times(2, 1, 1)_7"> </td>
        <td>25.9892</td>
        <td>v</td>
      </tr>
      <tr>
        <td> <img src="https://render.githubusercontent.com/render/math?math=ARIMA(2, 1, 1)\times(2, 1, 1)_7"> </td>
        <td>26.0168</td>
        <td></td>
      </tr>
    </table>

2. 前六百筆資料
     * non-seasonal:
        * acf cuts off after lag 1, pacf tails off; (p, q)=(0, 1)
    * seasonal:
        * acf cuts off after lag 1/2, pacf tails off; (P, Q)=[(0, 1), (0, 2)]
    <br>
    <table>
      <tr>
        <th>Model</th>
        <th>AIC</th>
        <th>lowest</th>
      </tr>
      <tr>
        <td> <img src="https://render.githubusercontent.com/render/math?math=ARIMA(0, 1, 1)\times(0, 1, 1)_7"> </td>
        <td>24.64652</td>
        <td></td>
      </tr>
      <tr>
        <td><img src="https://render.githubusercontent.com/render/math?math=ARIMA(0, 1, 1)\times(0, 1, 2)_7"></td>
        <td>24.64103</td>
        <td>v</td>
      </tr>
    </table>