import React, { useState }  from 'react';

import IconButton           from '../IconButton';
import panels, { PanelMap } from './panels';
import TextInput            from '../TextInput';
import styles               from './SearchSideBar.module.css';


interface Props {
    tags?:        [string, number][];
    initialPanel: string;
    children?:    PanelMap;
}

const SearchSideBar = (props: Props) => {
    const [search, changeSearch] = useState('');
    const [panel, changePanel]   = useState<string>(props.initialPanel);
    const panelMap: PanelMap     = {
        ...panels,
        ...props.children
    };

    const Current               = panelMap[panel].panel;

    return (
        <div className={styles.Root}>
            <TextInput
                value={search}
                placeholder="Search"
                onChange={(e) => changeSearch(e.target.value)}
                onKeyPress={(e) => {
                }}
            />

            <div className={styles.Buttons}>
                {
                    Object.values(panelMap).map(v =>
                        <IconButton
                            key={v.name}
                            icon={v.icon}
                            tooltip={v.tooltip}
                            active={panel === v.name}
                            onClick={() => changePanel(v.name)}
                        />
                    )
                }
            </div>

            <Current {...props} />
        </div>
    );
}

export default SearchSideBar;
