

# Module erlzord #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-config">config()</a> ###


__abstract datatype__: `config()`




### <a name="type-config">config()</a> ###


__abstract datatype__: `config()`




### <a name="type-coordinates">coordinates()</a> ###


<pre><code>
coordinates() = [integer()] | tuple()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#calculate-2">calculate/2</a></td><td></td></tr><tr><td valign="top"><a href="#calculate-3">calculate/3</a></td><td></td></tr><tr><td valign="top"><a href="#config-2">config/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="calculate-2"></a>

### calculate/2 ###

<pre><code>
calculate(Coordinates, Config) -&gt; Z
</code></pre>

<ul class="definitions"><li><code>Coordinates = <a href="#type-coordinates">coordinates()</a></code></li><li><code>Config = <a href="#type-config">config()</a></code></li><li><code>Z = non_neg_integer()</code></li></ul>

<a name="calculate-3"></a>

### calculate/3 ###

<pre><code>
calculate(Coordinates, MinCoordinateValue, MaxCoordinateValue) -&gt; Z
</code></pre>

<ul class="definitions"><li><code>Coordinates = <a href="#type-coordinates">coordinates()</a></code></li><li><code>MinCoordinateValue = integer()</code></li><li><code>MaxCoordinateValue = integer()</code></li><li><code>Z = non_neg_integer()</code></li></ul>

<a name="config-2"></a>

### config/2 ###

<pre><code>
config(MinCoordinateValue, MaxCoordinateValue) -&gt; Config
</code></pre>

<ul class="definitions"><li><code>MinCoordinateValue = integer()</code></li><li><code>MaxCoordinateValue = integer()</code></li><li><code>Config = <a href="#type-config">config()</a></code></li></ul>

