## Algorithm Viewer Help

The Algorithm Viewer allows you to visualize and explore clinical prediction algorithms by displaying odds ratio and predicted risk curves for various predictors.

### Getting Started

#### Uploading an Algorithm

1. In the left sidebar under the **Models** tab, click the **Browse** button next to "Upload Algorithm"
2. Select an archive file (.zip, .gz, .tar) containing your algorithm data
3. The archive file must contain exactly one YAML configuration file (`.yaml` or `.yml`) that defines the algorithm's models and variables
4. Once uploaded, the algorithm name and version will appear in the title bar

#### Selecting Models

After uploading an algorithm, the available models will appear as checkboxes under "Models:" in the sidebar.

- Check the boxes next to the models you want to visualize
- Multiple models can be selected simultaneously to compare them on the same plot
- Each model is displayed in a distinct color for easy identification

### Viewing Plots

The main panel contains tabs for different visualizations:

#### Odds Ratio Tab

Displays the odds ratio curve for the selected predictor across the selected models.

- The odds ratio shows how the risk changes relative to a reference value
- A dashed horizontal line at 1.0 indicates no change in risk
- Values above 1.0 indicate increased risk; values below 1.0 indicate decreased risk
- Hover over the plot to see exact values at each point

#### Predicted Risk Tab

Displays the predicted risk curve showing absolute risk values for the selected predictor.

- Shows the actual predicted probability/risk at each predictor value
- Useful for understanding the clinical impact of predictor changes

### Selecting Predictors

#### Primary Predictor

Use the **Predictor** dropdown to select the main variable to plot on the x-axis. The available predictors depend on which models are currently selected.

#### Interaction Predictor

The **Interaction Predictor** dropdown allows you to visualize how the effect of the primary predictor varies across different values of a second variable.

- Select `<empty>` to view only the primary predictor without interactions
- Select another variable to see how a one-unit increase in the variable affects the odds ratio. In this case, each curve uses the one-unit increase as the reference group.

### Adjusting Reference Groups

Click the **Reference** tab in the sidebar to adjust the reference group values used in calculations.

#### Using the Reference Controls

- Each model has its own set of reference value sliders
- **Continuous variables**: Use the numeric slider to set the reference value
- **Categorical variables**: Use the text slider to select from available categories
- The reference group defines the baseline against which odds ratios are calculated

#### Resetting Reference Values

Click the **Reset** button below each model's sliders to restore all reference values to their defaults.

### Plot Options

#### Logarithmic Scale

Toggle the **Logarithmic** checkbox to switch between:

- **Checked**: Logarithmic scale (useful for viewing a wide range of odds ratios)
- **Unchecked**: Linear scale (useful for seeing proportional differences)

### Tips

- Start by selecting a single model to understand its behavior before comparing multiple models
- Use the logarithmic scale when odds ratios span a wide range (e.g., 0.1 to 10)
- Adjust reference group values to see how the curves change with different baseline characteristics
- Hover over plot lines to see precise values at specific points

---

Tag: 0.1.0
