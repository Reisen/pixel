import React  from 'react';
import Toggle from '../../components/Toggle';


const SettingsPanel = () =>
    <div>
        <h1>Settings</h1>

        <Toggle on text="Hide NSFW" onPress={() => {}} />
        <Toggle on text="Hide Favorites" onPress={() => {}} />
        <Toggle on text="Only Untagged" onPress={() => {}} />
    </div>;

export default SettingsPanel;
