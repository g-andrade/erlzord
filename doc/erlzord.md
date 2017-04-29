

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
coordinates() = <a href="#type-list_coordinates">list_coordinates()</a> | <a href="#type-tuple_coordinates">tuple_coordinates()</a>
</code></pre>




### <a name="type-list_coordinates">list_coordinates()</a> ###


<pre><code>
list_coordinates() = [integer()]
</code></pre>




### <a name="type-tuple_coordinates">tuple_coordinates()</a> ###


<pre><code>
tuple_coordinates() = tuple()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#config-3">config/3</a></td><td></td></tr><tr><td valign="top"><a href="#decode-2">decode/2</a></td><td></td></tr><tr><td valign="top"><a href="#decode-4">decode/4</a></td><td></td></tr><tr><td valign="top"><a href="#encode-2">encode/2</a></td><td></td></tr><tr><td valign="top"><a href="#encode-4">encode/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="config-3"></a>

### config/3 ###

<pre><code>
config(Dimension, MinCoordinateValue, MaxCoordinateValue) -&gt; Config
</code></pre>

<ul class="definitions"><li><code>Dimension = non_neg_integer()</code></li><li><code>MinCoordinateValue = integer()</code></li><li><code>MaxCoordinateValue = integer()</code></li><li><code>Config = <a href="#type-config">config()</a></code></li></ul>

<a name="decode-2"></a>

### decode/2 ###

<pre><code>
decode(Z, Config) -&gt; Coordinates
</code></pre>

<ul class="definitions"><li><code>Z = non_neg_integer()</code></li><li><code>Config = <a href="#type-config">config()</a></code></li><li><code>Coordinates = <a href="#type-tuple_coordinates">tuple_coordinates()</a></code></li></ul>

<a name="decode-4"></a>

### decode/4 ###

<pre><code>
decode(Z, Dimension, MinCoordinateValue, MaxCoordinateValue) -&gt; Coordinates
</code></pre>

<ul class="definitions"><li><code>Z = non_neg_integer()</code></li><li><code>Dimension = non_neg_integer()</code></li><li><code>MinCoordinateValue = integer()</code></li><li><code>MaxCoordinateValue = integer()</code></li><li><code>Coordinates = <a href="#type-tuple_coordinates">tuple_coordinates()</a></code></li></ul>

<a name="encode-2"></a>

### encode/2 ###

<pre><code>
encode(Coordinates, Config) -&gt; Z
</code></pre>

<ul class="definitions"><li><code>Coordinates = <a href="#type-coordinates">coordinates()</a></code></li><li><code>Config = <a href="#type-config">config()</a></code></li><li><code>Z = non_neg_integer()</code></li></ul>

<a name="encode-4"></a>

### encode/4 ###

<pre><code>
encode(Coordinates, Dimension, MinCoordinateValue, MaxCoordinateValue) -&gt; Z
</code></pre>

<ul class="definitions"><li><code>Coordinates = <a href="#type-coordinates">coordinates()</a></code></li><li><code>Dimension = non_neg_integer()</code></li><li><code>MinCoordinateValue = integer()</code></li><li><code>MaxCoordinateValue = integer()</code></li><li><code>Z = non_neg_integer()</code></li></ul>

