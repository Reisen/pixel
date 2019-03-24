import TagPanel      from './TagPanel';
import MetaDataPanel from './MetaDataPanel';
import SettingsPanel  from './SettingsPanel';

export interface Panel {
    name:    string;
    icon:    string;
    tooltip: string;
    panel:   <Props>(props: Props) => JSX.Element;
}

export interface PanelMap {
    [index: string]: Panel
}

export {
    MetaDataPanel
}

export default {
    [TagPanel.name]:      TagPanel,
    [SettingsPanel.name]: SettingsPanel
};
