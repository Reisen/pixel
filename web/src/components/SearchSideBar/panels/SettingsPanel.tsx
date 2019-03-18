import React  from 'react';
import Toggle from '../../Toggle';


export default {
    name:    'settings',
    icon:    'gears',
    tooltip: 'Settings',
    panel:   () =>
        <div>
            <h1>Search Settings</h1>

            <Toggle on text="Hide NSFW" onPress={() => {}} />
            <Toggle on text="Hide Favorites" onPress={() => {}} />
            <Toggle on text="Only Untagged" onPress={() => {}} />
        </div>
};
